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

  let rec reduce_once exp =
    match exp with
    | Lambda.App ((Lambda.Lam (name, target), source)) ->
      (replace name target source, true)
    | Lambda.App (left, right) ->
      let (lxp, apped) = reduce_once left in
      if apped then
        (Lambda.App (lxp, right), apped)
      else
        let (rxp, apped) = reduce_once right in
        (Lambda.App (lxp, rxp), apped)
    | Lambda.Lam (name, exp) ->
      let (fxp, apped) = reduce_once exp in
      (Lambda.Lam (name, fxp), apped)
    | _ -> (exp, false)

  let rec reduce_simple exp =
    let (fxp, apped) = reduce_once (rename exp) in
    if apped then
      reduce_simple fxp
    else
      fxp

  let reduce : Lambda.lexp -> Lambda.lexp
  = fun exp -> reduce_simple exp

end
