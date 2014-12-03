(*
 * SNU 4190.310 Programming Languages (Fall 2012)
 *
 * Low Fat M: static type checking + interpreter without dynamic type checking
 *)

open M
module M_SimChecker : M_SimTypeChecker = struct
  type gtype =
    | GVar of int
    | GInt
    | GBool
    | GString
    | GPair of gtype * gtype
    | GLoc of gtype
    | GArrow of gtype * gtype

  let alpha = ref 0
  let init_alpha () =
    alpha := 0
  let next_alpha () =
    alpha := !alpha + 1;
    let v = !alpha in
    GVar v

  let subs_env s env =
    List.map (fun (k, v) -> (k, s v)) env

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
      (fun x -> x)
    else
      match (gl, gr) with
      | (GVar _, gr) ->
        if aing gl gr then
          raise (TypeError "unification fail (a, t)")
        else
          (fun x -> if x = gl then gr else x)
      | (gl, GVar _) ->
        if aing gr gl then
          raise (TypeError "unification fail (t, a)")
        else
          (fun x -> if x = gr then gl else x)
      | (GArrow (gl1, gl2), GArrow (gr1, gr2)) ->
        let s = unify gl1 gr1 in
        let s' = unify (s gl2) (s gr2) in
        (fun x -> s' (s x))
      | _ ->
        raise (TypeError "unification fail _")

  let rec g2t g =
    match g with
    | GVar n -> raise (TypeError "impossible")
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
      let a1 = next_alpha () in
      let a2 = next_alpha () in
      let g = GArrow (a1, a2) in
      let s = unify g typ in
      let senv = subs_env s env in
      let sa1 = s a1 in
      let sa2 = s a2 in
      let s' = sgm (env_push senv (x, sa1)) e sa2 in
      (fun x -> s' (s x))
    | APP (e, e') ->
      let a = next_alpha () in
      let s = sgm env e (GArrow (a, typ)) in
      let senv = subs_env s env in
      let sa = s a in
      let s' = sgm senv e' sa in
      (fun x -> s' (s x))
    | LET (REC (f, e), e') ->
      let a1 = next_alpha () in
      let a2 = next_alpha () in
      let g = GArrow (a1, a2) in
      let env' = env_push env (f, g) in
      let s = sgm env' e g in
      let senv = subs_env s env' in
      let s' = sgm senv e' typ in
      (fun x -> s' (s x))
    | LET (NREC (x, e), e') ->
      sgm env (APP (FN (x, e'), e)) typ
    | IF (ec, et, ef) ->
      let s = sgm env ec GBool in
      let senv = subs_env s env in
      let s' = sgm senv et typ in
      let senv' = subs_env s' senv in
      let styp = s' typ in
      let s'' = sgm senv' ef styp in
      (fun x -> s'' (s' (s x)))
    | BOP ((ADD|SUB), e1, e2) ->
      let s = unify GInt typ in
      let senv = subs_env s env in
      let s' = sgm senv e1 GInt in
      let senv' = subs_env s' senv in
      let s'' = sgm senv' e2 GInt in
      (fun x -> s'' (s' (s x)))
    | BOP ((AND|OR), e1, e2) ->
      let s = unify GBool typ in
      let senv = subs_env s env in
      let s' = sgm senv e1 GBool in
      let senv' = subs_env s' senv in
      let s'' = sgm senv' e2 GBool in
      (fun x -> s'' (s' (s x)))
    | BOP (EQ, e1, e2) ->
      raise (TypeError "not impl")
    | READ ->
      unify GInt typ
    | WRITE e ->
      raise (TypeError "not impl")
    | MALLOC e ->
      let a = next_alpha () in
      let g = GLoc a in
      let s = unify g typ in
      let senv = subs_env s env in
      let sa = s a in
      let s' = sgm senv e sa in
      (fun x -> s' (s x))
    | ASSIGN (el, er) ->
      let s = sgm env er typ in
      let senv = subs_env s env in
      let styp = s typ in
      let s' = sgm senv el (GLoc styp) in
      (fun x -> s' (s x))
    | BANG e ->
      sgm env e (GLoc typ)
    | SEQ (e1, e2) ->
      let a = next_alpha () in
      let s = sgm env e1 a in
      let senv = subs_env s env in
      let s' = sgm senv e2 typ in
      (fun x -> s' (s x))
    | PAIR (e1, e2) ->
      sgm env (FN ("#", APP (APP (VAR "#", e1), e2))) typ
    | SEL1 e ->
      sgm env (APP (e, FN ("x", FN ("y", VAR "x")))) typ
    | SEL2 e ->
      sgm env (APP (e, FN ("x", FN ("y", VAR "y")))) typ

  let check exp =
    init_alpha ();
    let a0 = next_alpha () in
    let s = sgm [] exp a0 in
    g2t (s a0)

end
