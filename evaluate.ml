(*
 * SNU 4190.310 Programming Languages (Fall 2014)
 *
 * Lambda Calculus
 *)

module Evaluator =
struct
  exception Error of string

  let rec all_names exp =
    match exp with
    | Lambda.Id x -> [x]
    | Lambda.Lam (x, e) -> x::(all_names e)
    | Lambda.App (l, r) -> List.rev_append (all_names l) (all_names r)

  let rename exp =
    let n = ref 0 in
    let names = all_names exp in
    let next_name () =
      let m = !n in
      n := m + 1;
      "s" ^ (string_of_int m)
    in
    let rec next_unique_name () =
      let name = next_name () in
      if List.mem name names then next_unique_name ()
      else name
    in
    let rec rename_in exp env =
      match exp with
      | Lambda.Id x -> (
        let sname = env x in
        match sname with
        | None -> exp
        | Some name -> Lambda.Id name
      )
      | Lambda.Lam (x, e) -> (
        let name = next_unique_name () in
        let env = fun y -> if y = x then Some name else env y in
        Lambda.Lam (name, rename_in e env)
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
