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

  let y =
    Lambda.Lam ("g",
      Lambda.App (
        Lambda.Lam ("x",
          Lambda.App (
            Lambda.Id "g",
            Lambda.App (Lambda.Id "x", Lambda.Id "x")
          )
        ),
        Lambda.Lam ("x",
          Lambda.App (
            Lambda.Id "g",
            Lambda.App (Lambda.Id "x", Lambda.Id "x")
          )
        )
      )
    )

  let t =
    Lambda.Lam ("x",
      Lambda.Lam ("y",
        Lambda.Id "x"
      )
    )

  let f =
    Lambda.Lam ("x",
      Lambda.Lam ("y",
        Lambda.Id "x"
      )
    )

  let isz =
    Lambda.Lam ("n",
      Lambda.App (
        Lambda.App (
          Lambda.Id "n",
          Lambda.Lam ("x", f)
        ),
        t
      )
    )

  let ifs =
    Lambda.Lam ("p",
      Lambda.Lam ("a",
        Lambda.Lam ("b",
          Lambda.App (
            Lambda.App (
              Lambda.Id "p",
              Lambda.Id "a"
            ),
            Lambda.Id "b"
          )
        )
      )
    )

  let succ =
    Lambda.Lam ("n",
      Lambda.Lam ("f",
        Lambda.Lam ("x",
          Lambda.App (
            Lambda.Id "f",
            Lambda.App (
              Lambda.App (
                Lambda.Id "n",
                Lambda.Id "f"
              ),
              Lambda.Id "x"
            )
          )
        )
      )
    )

  let rec encode : M.mexp -> Lambda.lexp
  = fun pgm -> match pgm with
  | M.Num n -> encode_num n
  | M.Var x -> Lambda.Id x
  | M.Fn (x, e) -> Lambda.Lam (x, encode e)
  | M.App (e1, e2) -> Lambda.App (encode e1, encode e2)
  | M.Rec (f, x, e) -> Lambda.App (y, Lambda.Lam (f, Lambda.Lam (x, encode e)))
  | M.Ifz (ep, ea, eb) ->
    Lambda.App (
      Lambda.App (
        Lambda.App (
          ifs,
          Lambda.App (
            isz,
            encode ep
          )
        ),
        encode ea
      ),
      encode eb
    )
  | M.Add (e1, e2) ->
    Lambda.Lam ("m",
      Lambda.Lam ("n",
        Lambda.App (
          Lambda.App (
            Lambda.Id "m",
            succ
          ),
          Lambda.Id "n"
        )
      )
    )
  | _ -> raise (Error "not implemented") (* Implement this *)

end
