(*
 * SNU 4190.310 Programming Languages (Fall 2012)
 *
 * Low Fat M: static type checking + interpreter without dynamic type checking
 *)

open M
module M_SimChecker : M_SimTypeChecker = struct
  type gtype =
    | GVar of gvar
    | GInt
    | GBool
    | GString
    | GPair of gtype * gtype
    | GLoc of gtype
    | GArrow of gtype * gtype
  and gvar =
    | GAll of int
    | GEquable of int
    | GWritable of int

  let alpha = ref 0
  let init_alpha () =
    alpha := 0
  let next_alpha () =
    alpha := !alpha + 1;
    let v = !alpha in
    v

  let rec subs_g_one (p, q) g =
    if g = p then
      q
    else
      match g with
      | GPair (g1, g2) ->
        GPair (subs_g_one (p, q) g1, subs_g_one (p, q) g2)
      | GLoc g0 ->
        GLoc (subs_g_one (p, q) g0)
      | GArrow (g1, g2) ->
        GArrow (subs_g_one (p, q) g1, subs_g_one (p, q) g2)
      | _ -> g

  let subs_g s g =
    List.fold_left
      (fun g (p, q) -> subs_g_one (p, q) g)
      g
      (List.rev s)

  let subs_env s env =
    List.map (fun (k, v) -> (k, subs_g s v)) env

  let env_push env (x, g) =
    (x, g)::env

  let env_lookup env x =
    try
      let (_, g) = List.find (fun (k, v) -> k = x) env in g
    with
      Not_found -> raise (TypeError "free variable")

  let rec aing a g =
    if a = g then
      true
    else
      match g with
      | GPair (g1, g2) -> (aing a g1) || (aing a g2)
      | GLoc g0 -> aing a g0
      | GArrow (g1, g2) -> (aing a g1) || (aing a g2)
      | _ -> false

  let rec unify gl gr =
    if gl = gr then
      []
    else
      match (gl, gr) with
      | (GVar (GAll _), _)
      | (
          GVar (GEquable _),
          (
            GInt | GBool | GString | (GLoc _) |
            GVar (GWritable _) | GVar (GEquable _)
          )
        )
      | (
          GVar (GWritable _),
          (
            GInt | GBool | GString |
            GVar (GWritable _)
          )
        ) ->
        if aing gl gr then
          raise (TypeError "unification fail (a, t)")
        else
          [(gl, gr)]
      | (_, GVar (GAll _))
      | (
          (
            GInt | GBool | GString | (GLoc _) |
            GVar (GWritable _)
          ),
          GVar (GEquable _)
        )
      | (
          (
            GInt | GBool | GString
          ),
          GVar (GWritable _)
        ) ->
        if aing gr gl then
          raise (TypeError "unification fail (t, a)")
        else
          [(gr, gl)]
      | (GLoc gl0, GLoc gr0) ->
        unify gl0 gr0
      | (GPair (gl1, gl2), GPair (gr1, gr2))
      | (GArrow (gl1, gl2), GArrow (gr1, gr2)) ->
        let s = unify gl1 gr1 in
        let s' = unify (subs_g s gl2) (subs_g s gr2) in
        s' @ s
      | _ ->
        raise (TypeError "unification fail _")

  let rec g2t g =
    match g with
    | GVar _ -> raise (TypeError "impossible")
    | GInt -> TyInt
    | GBool -> TyBool
    | GString -> TyString
    | GPair (t0, t1) -> TyPair (g2t t0, g2t t1)
    | GLoc t0 -> TyLoc (g2t t0)
    | GArrow (t0, t1) -> TyArrow (g2t t0, g2t t1)

  let rec sgm env exp typ =
    match exp with
    | CONST (S _) ->
      unify GString typ
    | CONST (N _) ->
      unify GInt typ
    | CONST (B _) ->
      unify GBool typ
    | VAR x ->
      unify typ (env_lookup env x)
    | FN (x, e) ->
      let a1 = GVar (GAll (next_alpha ())) in
      let a2 = GVar (GAll (next_alpha ())) in
      let g = GArrow (a1, a2) in
      let s = unify g typ in
      let senv = subs_env s env in
      let sa1 = subs_g s a1 in
      let sa2 = subs_g s a2 in
      let s' = sgm (env_push senv (x, sa1)) e sa2 in
      s' @ s
    | APP (e, e') ->
      let a = GVar (GAll (next_alpha ())) in
      let s = sgm env e (GArrow (a, typ)) in
      let senv = subs_env s env in
      let sa = subs_g s a in
      let s' = sgm senv e' sa in
      s' @ s
    | LET (REC (f, e), e') ->
      let a1 = GVar (GAll (next_alpha ())) in
      let a2 = GVar (GAll (next_alpha ())) in
      let env = env_push env (f, GArrow (a1, a2)) in
      let s = unify a2 typ in
      let senv = subs_env s env in
      let sa1 = subs_g s a1 in
      let sa2 = subs_g s a2 in
      let s' = sgm senv e' sa1 in
      let senv' = subs_env s' senv in
      let sg' = subs_g s' (GArrow (sa1, sa2)) in
      let s'' = sgm senv' e sg' in
      s'' @ s' @ s
    | LET (NREC (x, e), e') ->
      sgm env (APP (FN (x, e'), e)) typ
    | IF (ec, et, ef) ->
      let s = sgm env ec GBool in
      let senv = subs_env s env in
      let s' = sgm senv et typ in
      let senv' = subs_env s' senv in
      let styp = subs_g s' typ in
      let s'' = sgm senv' ef styp in
      s'' @ s' @ s
    | BOP ((ADD|SUB), e1, e2) ->
      let s = unify GInt typ in
      let senv = subs_env s env in
      let s' = sgm senv e1 GInt in
      let senv' = subs_env s' senv in
      let s'' = sgm senv' e2 GInt in
      s'' @ s' @ s
    | BOP ((AND|OR), e1, e2) ->
      let s = unify GBool typ in
      let senv = subs_env s env in
      let s' = sgm senv e1 GBool in
      let senv' = subs_env s' senv in
      let s'' = sgm senv' e2 GBool in
      s'' @ s' @ s
    | BOP (EQ, e1, e2) ->
      let s = unify GBool typ in
      let senv = subs_env s env in
      let a = GVar (GEquable (next_alpha ())) in
      let s' = sgm senv e1 a in
      let senv' = subs_env s' senv in
      let sa' = subs_g s' a in
      let s'' = sgm senv' e2 sa' in
      s'' @ s' @ s
    | READ ->
      unify GInt typ
    | WRITE e ->
      let a = GVar (GWritable (next_alpha ())) in
      let s = unify a typ in
      let senv = subs_env s env in
      let styp = subs_g s typ in
      let s' = sgm senv e styp in
      s' @ s
    | MALLOC e ->
      let a = GVar (GAll (next_alpha ())) in
      let g = GLoc a in
      let s = unify g typ in
      let senv = subs_env s env in
      let sa = subs_g s a in
      let s' = sgm senv e sa in
      s' @ s
    | ASSIGN (el, er) ->
      let s = sgm env er typ in
      let senv = subs_env s env in
      let styp = subs_g s typ in
      let s' = sgm senv el (GLoc styp) in
      s' @ s
    | BANG e ->
      sgm env e (GLoc typ)
    | SEQ (e1, e2) ->
      let a = GVar (GAll (next_alpha ())) in
      let s = sgm env e1 a in
      let senv = subs_env s env in
      let s' = sgm senv e2 typ in
      s' @ s
    | PAIR (e1, e2) ->
      let a1 = GVar (GAll (next_alpha ())) in
      let a2 = GVar (GAll (next_alpha ())) in
      let g = GPair (a1, a2) in
      let s = unify g typ in
      let senv = subs_env s env in
      let sa1 = subs_g s a1 in
      let sa2 = subs_g s a2 in
      let s' = sgm senv e1 sa1 in
      let senv' = subs_env s' senv in
      let sa2' = subs_g s' sa2 in
      let s'' = sgm senv' e2 sa2' in
      s'' @ s' @ s
    | SEL1 e ->
      let a1 = GVar (GAll (next_alpha ())) in
      let a2 = GVar (GAll (next_alpha ())) in
      let g = GPair (a1, a2) in
      let s = unify a1 typ in
      let senv = subs_env s env in
      let sg = subs_g s g in
      let s' = sgm senv e sg in
      s' @ s
    | SEL2 e ->
      let a1 = GVar (GAll (next_alpha ())) in
      let a2 = GVar (GAll (next_alpha ())) in
      let g = GPair (a1, a2) in
      let s = unify a2 typ in
      let senv = subs_env s env in
      let sg = subs_g s g in
      let s' = sgm senv e sg in
      s' @ s

  let check exp =
    init_alpha ();
    let a0 = GVar (GAll (next_alpha ())) in
    let s = sgm [] exp a0 in
    g2t (subs_g s a0)

end
