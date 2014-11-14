(*
 * SNU 4190.310 Programming Languages 
 *
 * Sonata
 *)
open Sm5
open Sonata
module Rozetta = struct

  let rec trans_tail command result =
    match command with
    | [] -> result
    | (Sm5.PUSH (Sm5.Val (Sm5.Z n)))::tail ->
      trans_tail tail
        ((Sonata.PUSH (Sonata.Val (Sonata.Z n)))::result)
    | (Sm5.PUSH (Sm5.Val (Sm5.B b)))::tail ->
      trans_tail tail
        ((Sonata.PUSH (Sonata.Val (Sonata.B b)))::result)
    | (Sm5.PUSH (Sm5.Val (Sm5.L l)))::tail ->
      failwith "l"
    | (Sm5.PUSH (Sm5.Val Sm5.Unit))::tail ->
      trans_tail tail
        ((Sonata.PUSH (Sonata.Val Sonata.Unit))::result)
    | (Sm5.PUSH (Sm5.Val (Sm5.R r)))::tail ->
      failwith "r"
    | (Sm5.PUSH (Sm5.Id x))::tail ->
      trans_tail tail
        ((Sonata.PUSH (Sonata.Id x))::result)
    | (Sm5.PUSH (Sm5.Fn (x, c)))::tail ->
      let c' = trans_tail c Sonata.empty_command in
      trans_tail tail
        ((Sonata.PUSH (Sonata.Fn (x, c')))::result)
    | (Sm5.POP)::tail ->
      trans_tail tail
        ((Sonata.POP)::result)
    | (Sm5.STORE)::tail ->
      trans_tail tail
        ((Sonata.STORE)::result)
    | (Sm5.LOAD)::tail ->
      trans_tail tail
        ((Sonata.LOAD)::result)
    | (Sm5.JTR (ct, cf))::tail ->
      let ct' = trans_tail ct Sonata.empty_command in
      let cf' = trans_tail cf Sonata.empty_command in
      trans_tail tail
        ((Sonata.JTR (ct', cf'))::result)

  let trans : Sm5.command -> Sonata.command = fun command ->
    trans_tail command Sonata.empty_command

end
