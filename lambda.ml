(*
 * SNU 4190.310 Programming Languages 
 *
 * Lambda Calculus
 *)

type lexp = Id of string
		  | Lam of string * lexp
		  | App of lexp * lexp
