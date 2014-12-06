(*
 * SNU 4190.310 Programming Languages (Fall 2012)
 *
 * Let-polymorphism with M algorithm
 *)
open M

module M_PolyChecker : M_PolyChecker = struct
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
  and gscheme =
    | GSimple of gtype
    | GPoly of gvar * gscheme

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

  let subs_except s a =
    List.filter (fun (p, _) -> p != a) s
  let rec subs_gs s gs =
    match gs with
    | GSimple g ->
      GSimple (subs_g s g)
    | GPoly (a, gs') ->
      GPoly (a, subs_gs (subs_except s (GVar a)) gs')

  let subs_env s env =
    List.map (fun (k, v) -> (k, subs_gs s v)) env

  let env_push env (x, gs) =
    (x, gs)::env

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

  let rec spe_in s gs =
    match gs with
    | GSimple g ->
      subs_g s g
    | GPoly (GAll n, gs') ->
      spe_in
        ((
          GVar (GAll n),
          GVar (GAll (next_alpha ()))
        )::s)
        gs'
    | GPoly (GEquable n, gs') ->
      spe_in
        ((
          GVar (GEquable n),
          GVar (GEquable (next_alpha ()))
        )::s)
        gs'
    | GPoly (GWritable n, gs') ->
      spe_in
        ((
          GVar (GWritable n),
          GVar (GWritable (next_alpha ()))
        )::s)
        gs'
  let spe gs =
    spe_in [] gs

  let diff l1 l2 =
    List.filter
      (fun v -> not (List.mem v l2))
      l1
  let rec ftv_g g =
    match g with
    | GVar v ->
      [v]
    | GInt | GBool | GString ->
      []
    | GPair (g1, g2) | GArrow (g1, g2) ->
      (ftv_g g1) @ (ftv_g g2)
    | GLoc g0 ->
      ftv_g g0
  let rec ftv_gs_in gs al =
    match gs with
    | GSimple g ->
      diff (ftv_g g) al
    | GPoly (a, gs') ->
      ftv_gs_in gs' (a::al)
  let ftv_gs gs =
    ftv_gs_in gs []
  let rec ftv_env env =
    List.fold_left
      (fun l (_, gs) -> l @ (ftv_gs gs))
      []
      env
  let rec gen_in g al =
    match al with
    | [] ->
      GSimple g
    | a::al' ->
      GPoly (a, gen_in g al')
  let gen env g =
    let al = diff (ftv_g g) (ftv_env env) in
    gen_in g al

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
    | GVar _ -> raise (TypeError "unable to express")
    | GInt -> TyInt
    | GBool -> TyBool
    | GString -> TyString
    | GPair (t0, t1) -> TyPair (g2t t0, g2t t1)
    | GLoc t0 -> TyLoc (g2t t0)
    | GArrow (t0, t1) -> TyArrow (g2t t0, g2t t1)

  let rec expansive e =
    match e with
    | CONST _ | VAR _ | FN _ | READ -> false
    | APP _ | MALLOC _ -> true
    | LET (REC (_, e1), e2) | LET (NREC (_, e1), e2)
    | BOP (_, e1, e2) | ASSIGN (e1, e2)
    | SEQ (e1, e2) | PAIR (e1, e2) ->
      (expansive e1) || (expansive e2)
    | IF (ec, et, ef) ->
      (expansive ec) || (expansive et) || (expansive ef)
    | WRITE e0 | BANG e0 | SEL1 e0 | SEL2 e0 ->
      expansive e0

  let rec sgm env exp typ =
    match exp with
    | CONST (S _) ->
      unify GString typ
    | CONST (N _) ->
      unify GInt typ
    | CONST (B _) ->
      unify GBool typ
    | VAR x ->
      unify typ (spe (env_lookup env x))
    | FN (x, e) ->
      let a1 = GVar (GAll (next_alpha ())) in
      let a2 = GVar (GAll (next_alpha ())) in
      let g = GArrow (a1, a2) in
      let s = unify g typ in
      let senv = subs_env s env in
      let sa1 = subs_g s a1 in
      let sa2 = subs_g s a2 in
      let s' = sgm (env_push senv (x, (GSimple sa1))) e sa2 in
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
      let g = GArrow (a1, a2) in
      let env' = env_push env (f, GSimple g) in
      let s = sgm env' e g in
      let senv = subs_env s env in
      let sg = subs_g s g in
      let styp = subs_g s typ in
      let gsg =
        if expansive e then (GSimple sg)
        else gen senv sg in
      let s' = sgm (env_push senv (f, gsg)) e' styp in
      s' @ s
    | LET (NREC (x, e), e') ->
      let a = GVar (GAll (next_alpha ())) in
      let s = sgm env e a in
      let senv = subs_env s env in
      let sa = subs_g s a in
      let styp = subs_g s typ in
      let gsa =
        if expansive e then (GSimple sa)
        else gen senv sa in
      let s' = sgm (env_push senv (x, gsa)) e' styp in
      s' @ s
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
    let exp' = LET (NREC ("#", exp), VAR "#") in
    let s = sgm [] exp' a0 in
    let g = subs_g s a0 in
    g2t g

end
