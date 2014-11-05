(*
 * SNU 4190.310 Programming Languages (Fall 2014)
 *
 * SM5
 *)
open K
open Sm5
module Translator = struct

let tail_concat ll =
  List.rev (
    List.fold_left
      (fun res l -> List.rev_append l res)
      [] ll
  )

let rec trans : K.program -> Sm5.command
= fun pgm ->
  match pgm with
  | K.NUM n -> [Sm5.PUSH (Sm5.Val (Sm5.Z n))]
  | K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))]
  | K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
  | K.UNIT -> [Sm5.PUSH (Sm5.Val Sm5.Unit)]
  | K.VAR x -> [Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
  | K.ADD (e1, e2) ->
    tail_concat [
      (trans e1);
      (trans e2);
      [Sm5.ADD];
    ]
  | K.SUB (e1, e2) ->
    tail_concat [
      (trans e1);
      (trans e2);
      [Sm5.SUB];
    ]
  | K.MUL (e1, e2) ->
    tail_concat [
      (trans e1);
      (trans e2);
      [Sm5.MUL];
    ]
  | K.DIV (e1, e2) ->
    tail_concat [
      (trans e1);
      (trans e2);
      [Sm5.DIV];
    ]
  | K.EQUAL (e1, e2) ->
    tail_concat [
      (trans e1);
      (trans e2);
      [Sm5.EQ];
    ]
  | K.LESS (e1, e2) ->
    tail_concat [
      (trans e1);
      (trans e2);
      [Sm5.LESS];
    ]
  | K.NOT e ->
    tail_concat [
      (trans e);
      [Sm5.NOT];
    ]
  | K.ASSIGN (x, e) ->
    tail_concat [
      (trans e);
      [
        Sm5.PUSH (Sm5.Id x);
        Sm5.STORE;
        Sm5.PUSH (Sm5.Id x);
        Sm5.LOAD;
      ];
    ]
  | K.SEQ (e1, e2) ->
    tail_concat [
      (trans e1);
      [Sm5.POP];
      (trans e2);
    ]
  | K.IF (ec, et, ef) ->
    tail_concat [
      (trans ec);
      [Sm5.JTR (trans et, trans ef)];
    ]
  | K.WHILE (ec, e) ->
    [
      Sm5.MALLOC;
      Sm5.BIND "0v";
      Sm5.PUSH (
        Sm5.Fn (
          "0v",
          tail_concat [
            [Sm5.BIND "0f"];
            (trans ec);
            [
              Sm5.JTR (
                tail_concat [
                  (trans e);
                  [
                    Sm5.POP;
                    Sm5.PUSH (Sm5.Id "0f");
                    Sm5.PUSH (Sm5.Id "0f");
                    Sm5.UNBIND;
                    Sm5.POP;
                    Sm5.PUSH (Sm5.Val Sm5.Unit);
                    Sm5.PUSH (Sm5.Id "0v");
                    Sm5.CALL;
                  ]
                ],
                [
                  Sm5.UNBIND;
                  Sm5.POP;
                  Sm5.PUSH (Sm5.Val Sm5.Unit);
                ]
              )
            ]
          ]
        )
      );
      Sm5.BIND "0f";
      Sm5.PUSH (Sm5.Id "0f");
      Sm5.PUSH (Sm5.Id "0f");
      Sm5.UNBIND;
      Sm5.POP;
      Sm5.PUSH (Sm5.Val Sm5.Unit);
      Sm5.PUSH (Sm5.Id "0v");
      Sm5.CALL;
      Sm5.UNBIND;
      Sm5.POP;
    ]
  | K.FOR (x, eb, ee, e) ->
    tail_concat [
      (trans eb);
      (trans ee);
      [
        Sm5.BIND "2";
        Sm5.BIND "1";
        Sm5.PUSH (Sm5.Id "2");
        Sm5.PUSH (Sm5.Id "1");
        Sm5.UNBIND;
        Sm5.POP;
        Sm5.UNBIND;
        Sm5.POP;
        Sm5.PUSH (
          Sm5.Fn (
            x,
            [
              Sm5.BIND "0f";
              Sm5.BIND "1";
              Sm5.BIND "2";
              Sm5.PUSH (Sm5.Id "2");
              Sm5.PUSH (Sm5.Id "1");
              Sm5.PUSH (Sm5.Id "2");
              Sm5.PUSH (Sm5.Id "1");
              Sm5.UNBIND;
              Sm5.POP;
              Sm5.UNBIND;
              Sm5.POP;
              Sm5.LESS;
              Sm5.JTR (
                [
                  Sm5.UNBIND;
                  Sm5.POP;
                  Sm5.POP;
                  Sm5.POP;
                  Sm5.PUSH (Sm5.Val Sm5.Unit);
                ],
                tail_concat [
                  [
                    Sm5.BIND "1";
                    Sm5.PUSH (Sm5.Id "1");
                    Sm5.PUSH (Sm5.Id "1");
                    Sm5.UNBIND;
                    Sm5.POP;
                    Sm5.PUSH (Sm5.Id x);
                    Sm5.STORE;
                  ];
                  (trans e);
                  [
                    Sm5.POP;
                    Sm5.PUSH (Sm5.Val (Sm5.Z 1));
                    Sm5.ADD;
                    Sm5.PUSH (Sm5.Id "0f");
                    Sm5.PUSH (Sm5.Id "0f");
                    Sm5.UNBIND;
                    Sm5.POP;
                    Sm5.PUSH (Sm5.Id x);
                    Sm5.LOAD;
                    Sm5.PUSH (Sm5.Id x);
                    Sm5.CALL;
                  ];
                ]
              );
            ]
          )
        );
        Sm5.BIND "0f";
        Sm5.PUSH (Sm5.Id "0f");
        Sm5.PUSH (Sm5.Id "0f");
        Sm5.UNBIND;
        Sm5.POP;
        Sm5.PUSH (Sm5.Id x);
        Sm5.LOAD;
        Sm5.PUSH (Sm5.Id x);
        Sm5.CALL;
      ]
    ]
  | K.LETV (x, e, es) ->
    tail_concat [
      (trans e);
      [
        Sm5.MALLOC;
        Sm5.BIND x;
        Sm5.PUSH (Sm5.Id x);
        Sm5.STORE;
      ];
      (trans es);
      [
        Sm5.UNBIND;
        Sm5.POP;
      ];
    ]
  | K.LETF (f, x, e, es) ->
    tail_concat [
      [
        Sm5.PUSH (Sm5.Fn (x, trans e));
        Sm5.BIND f;
      ];
      (trans es);
      [
        Sm5.UNBIND;
        Sm5.POP;
      ];
    ]
  | K.CALLV (f, e) ->
    tail_concat [
      [Sm5.PUSH (Sm5.Id f)];
      (trans e);
      [
        Sm5.MALLOC;
        Sm5.CALL;
      ];
    ]
  | K.CALLR (f, x) ->
    [
      Sm5.PUSH (Sm5.Id f);
      Sm5.PUSH (Sm5.Id x);
      Sm5.LOAD;
      Sm5.PUSH (Sm5.Id x);
      Sm5.CALL;
    ]
  | K.READ x ->
    [
      Sm5.GET;
      Sm5.PUSH (Sm5.Id x);
      Sm5.STORE;
      Sm5.PUSH (Sm5.Id x);
      Sm5.LOAD;
    ]
  | K.WRITE e ->
    tail_concat [
      (trans e);
      [
        Sm5.BIND "0";
        Sm5.PUSH (Sm5.Id "0");
        Sm5.PUSH (Sm5.Id "0");
        Sm5.UNBIND;
        Sm5.POP;
        Sm5.PUT;
      ];
    ]

end
