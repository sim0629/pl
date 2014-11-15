(*
 * SNU 4190.310 Programming Languages 
 *
 * SM5
 *)
open K
open Sm5
module Translator = struct
let rec trans : K.program -> Sm5.command
= fun pgm -> match pgm with
  | K.NUM i -> [Sm5.PUSH(Sm5.Val(Sm5.Z i))]
  | K.TRUE -> [Sm5.PUSH(Sm5.Val(Sm5.B true))]
  | K.FALSE -> [Sm5.PUSH(Sm5.Val(Sm5.B false))]
  | K.UNIT -> [Sm5.PUSH(Sm5.Val Sm5.Unit)]
  | K.VAR id -> [Sm5.PUSH(Sm5.Id id);Sm5.LOAD]
  | K.ADD(e1,e2) -> trans e1@trans e2@[Sm5.ADD]
  | K.SUB(e1,e2) -> trans e1@trans e2@[Sm5.SUB]
  | K.MUL(e1,e2) -> trans e1@trans e2@[Sm5.MUL]
  | K.DIV(e1,e2) -> trans e1@trans e2@[Sm5.DIV]
  | K.EQUAL(e1,e2) -> trans e1@trans e2@[Sm5.EQ]
  | K.LESS(e1,e2) -> trans e1@trans e2@[Sm5.LESS]
  | K.NOT e -> trans e@[Sm5.NOT]
  | K.ASSIGN(id,e) -> trans e@[Sm5.PUSH(Sm5.Id id);Sm5.STORE;Sm5.PUSH(Sm5.Id id);Sm5.LOAD]
  | K.SEQ(e1,e2) -> trans e1@(Sm5.POP::trans e2)
  | K.IF(c,t,f) -> trans c@[Sm5.JTR(trans t, trans f)]
  | K.WHILE(c,body) ->
    trans (
      K.LETF("while","do",
        K.IF(c,
          K.SEQ(body,K.CALLR("while","do")),
          K.UNIT),
        K.CALLV("while",K.UNIT)))
  | K.FOR(x,bot,top,body) -> 
    trans ( 
      K.SEQ(K.ASSIGN(x, bot), 
        K.LETV("to", top, 
          K.LETF("for","do", 
            K.IF(K.LESS(K.VAR "do", K.ADD(K.VAR "to", K.NUM 1)), 
              K.SEQ(K.ASSIGN(x, K.VAR "do"), 
                K.SEQ(body,                
                   K.CALLV("for", K.ADD(K.VAR "do", K.NUM 1)))), 
              K.UNIT), 
            K.CALLV("for", K.VAR x)))))  
  | K.LETV(x,e1,e2) ->
    trans e1
    @[Sm5.MALLOC;Sm5.BIND x;Sm5.PUSH(Sm5.Id x);Sm5.STORE]
    @trans e2@[Sm5.UNBIND;Sm5.POP]
  | K.LETF(f,x,e1,e2) ->
    let body = (Sm5.BIND f::trans e1)@[Sm5.UNBIND;Sm5.POP] in
    [Sm5.PUSH(Sm5.Fn(x,body));Sm5.BIND f]@trans e2@[Sm5.UNBIND;Sm5.POP]
  | K.CALLV(f,e) ->
    (Sm5.PUSH(Sm5.Id f)::Sm5.PUSH(Sm5.Id f)::trans e)@[Sm5.MALLOC;Sm5.CALL]
  | K.CALLR(f,x) ->
    [Sm5.PUSH(Sm5.Id f);Sm5.PUSH(Sm5.Id f);Sm5.PUSH(Sm5.Id x);Sm5.LOAD;Sm5.PUSH(Sm5.Id x);Sm5.CALL]
  | K.READ x -> [Sm5.GET;Sm5.PUSH(Sm5.Id x);Sm5.STORE;Sm5.PUSH(Sm5.Id x);Sm5.LOAD]
  | K.WRITE e ->
    [Sm5.MALLOC;Sm5.BIND "write"]
    @trans e
    @[Sm5.PUSH(Sm5.Id "write");Sm5.STORE;Sm5.PUSH(Sm5.Id "write");Sm5.LOAD;Sm5.PUT;Sm5.PUSH(Sm5.Id "write");Sm5.LOAD;Sm5.UNBIND;Sm5.POP]
end
