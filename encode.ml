(*
 * SNU 4190.310 Programming Languages
 *
 * M0
 *)
open M
module Encoder =
struct
  exception Error of string

  let encode_num n =
    let rec encode_num_in n result =
      if n = 0 then
        result
      else if n < 0 then
        raise (Error "negative integer")
      else
        encode_num_in (n - 1) (Lambda.App (Lambda.Id "f", result))
    in
    Lambda.Lam ("f",
      Lambda.Lam ("x",
        encode_num_in n (Lambda.Id "x")
      )
    )

  let rec encode : M.mexp -> Lambda.lexp
  = fun pgm -> match pgm with
  | M.Num n -> encode_num n
  | M.Var x -> Lambda.Id x
  | M.Fn (x, e) -> Lambda.Lam (x, encode e)
  | _ -> raise (Error "not implemented") (* Implement this *)

end
