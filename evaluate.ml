(*
 * SNU 4190.310 Programming Languages (Fall 2014)
 *
 * Lambda Calculus
 *)

module Evaluator =
struct
  exception Error of string

  let rename exp =
    let n = ref 0 in
    let rec rename_in exp env =
      match exp with
      | Lambda.Id x -> (
        let n = env x in
        match n with
        | None -> exp
        | Some i -> Lambda.Id (string_of_int i)
      )
      | Lambda.Lam (x, e) -> (
        let m = !n in
        n := m + 1;
        let env = fun y -> if y = x then Some m else env y in
        Lambda.Lam (string_of_int m, rename_in e env)
      )
      | Lambda.App (l, r) -> (
        Lambda.App (rename_in l env, rename_in r env)
      )
    in
    rename_in exp (fun y -> None)

  let rec replace name target source =
    match target with
    | Lambda.Id x ->
      if x = name then source else target
    | Lambda.Lam (x, e) ->
      Lambda.Lam (x, replace name e source)
    | Lambda.App (l, r) ->
      Lambda.App (replace name l source, replace name r source)

  let rec reduce_simple exp =
    match exp with
    | Lambda.App ((Lambda.Lam (name, target), source)) ->
      reduce_simple (replace name target source)
    | Lambda.App (left, right) ->
      let fxp = Lambda.App (reduce_simple left, reduce_simple right) in
      if exp = fxp then fxp
      else reduce_simple fxp
    | Lambda.Lam (name, exp) ->
      Lambda.Lam (name, reduce_simple exp)
    | _ -> exp

  let reduce : Lambda.lexp -> Lambda.lexp
  = fun exp -> reduce_simple (rename exp)

end
