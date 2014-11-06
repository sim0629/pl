(*
 * SNU 4190.310 Programming Languages 
 *
 * SM5
 *)
open K
open Sm5

module Translator = struct
  let rec trans : K.program -> Sm5.command = fun k_pgm ->
    raise (Failure "Translator.trans() unimplemented, substitute this translate_k.ml file with your translate.ml file from SM5 homework.")
end
