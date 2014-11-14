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
      let c' = trans c in
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
      let ct' = trans ct in
      let cf' = trans cf in
      trans_tail tail
        ((Sonata.JTR (ct', cf'))::result)
    | (Sm5.MALLOC)::tail ->
      trans_tail tail
        ((Sonata.MALLOC)::result)
    | (Sm5.BOX n)::tail ->
      trans_tail tail
        ((Sonata.BOX n)::result)
    | (Sm5.UNBOX x)::tail ->
      trans_tail tail
        ((Sonata.UNBOX x)::result)
    | (Sm5.BIND x)::tail ->
      trans_tail tail
        ((Sonata.BIND x)::result)
    | (Sm5.UNBIND)::tail ->
      trans_tail tail
        ((Sonata.UNBIND)::result)
    | (Sm5.GET)::tail ->
      trans_tail tail
        ((Sonata.GET)::result)
    | (Sm5.PUT)::tail ->
      trans_tail tail
        ((Sonata.PUT)::result)
    | (Sm5.CALL)::tail ->
      failwith "not implemented"
    | (Sm5.ADD)::tail ->
      trans_tail tail
        ((Sonata.ADD)::result)
    | (Sm5.SUB)::tail ->
      trans_tail tail
        ((Sonata.SUB)::result)
    | (Sm5.MUL)::tail ->
      trans_tail tail
        ((Sonata.MUL)::result)
    | (Sm5.DIV)::tail ->
      trans_tail tail
        ((Sonata.DIV)::result)
    | (Sm5.EQ)::tail ->
      trans_tail tail
        ((Sonata.EQ)::result)
    | (Sm5.LESS)::tail ->
      trans_tail tail
        ((Sonata.LESS)::result)
    | (Sm5.NOT)::tail ->
      trans_tail tail
        ((Sonata.NOT)::result)
  and trans : Sm5.command -> Sonata.command = fun command ->
    List.rev (trans_tail command Sonata.empty_command)

end
