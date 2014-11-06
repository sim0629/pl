(*
 * SNU 4190.310 Programming Languages 
 *
 * M0
 *)
open M
module Encoder = 
  struct
  	exception Error of string

	let encode : M.mexp -> Lambda.lexp
	= fun pgm -> raise (Error "not implemented") (* Implement this *)

  end
