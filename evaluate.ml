(*
 * SNU 4190.310 Programming Languages (Fall 2014)
 *
 * Lambda Calculus
 *)

module Evaluator =
struct
  exception Error of string

  let rec rename used exp =
    let rec rename_in name =
      if List.mem name used then rename_in (name ^ "s")
      else name
    in
    match exp with
    | Lambda.Id x -> Lambda.Id (rename_in x)
    | Lambda.Lam (x, e) -> Lambda.Lam (x, rename (List.filter (fun y -> y <> x) used) e)
    | Lambda.App (l, r) -> Lambda.App (rename used l, rename used r)

  let rec replace name target source used =
    match target with
    | Lambda.Id x ->
      if x = name then source
      else target
    | Lambda.Lam (x, e) ->
      if x = name then target
      else Lambda.Lam (x, replace name e (rename (x::used) source) (x::used))
    | Lambda.App (l, r) ->
      Lambda.App (replace name l source used, replace name r source used)

  let rec reduce : Lambda.lexp -> Lambda.lexp
  = fun exp ->
    match exp with
    | Lambda.App ((Lambda.Lam (name, target), source)) ->
      reduce (replace name target source [])
    | Lambda.App (left, right) ->
      Lambda.App (reduce left, reduce right)
    | _ ->
      exp

end
