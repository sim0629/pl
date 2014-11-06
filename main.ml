(*
 * SNU 4190.310 Programming Languages 
 *
 * SM5
 *)

open Rozetta
open Pp
open K
open Translate_k
open Sm5
open Sm5
open Sonata

let main () =
    let pp = ref false in
	let psm5 = ref false in
	let psonata = ref false in
	let k = ref false in
	let sm5 = ref false in
	let sonata = ref false in
    let src = ref "" in
    let _ =
        Arg.parse
          [("-pp", Arg.Set pp, "display parse tree");
		   ("-psm5", Arg.Set psm5, "print translated sm5 code");
		   ("-psonata", Arg.Set psonata, "print translated sonata code");
           ("-k", Arg.Set k, "run using k interpreter");
		   ("-sm5", Arg.Set sm5, "translate k-- to sm5 and run using sm5 interpreter");
		   ("-sonata", Arg.Set sonata, "translate k-- to sm5 and then to sonata, and run using sonata interpreter")]
          (fun x -> src := x)
          ("Usage: " ^ (Filename.basename Sys.argv.(0)) ^ " [-pp | -psm5 | -psonata | -k | -sm5 | -sonata] [file]")
    in

    let read_program () = 
        let lexbuf = Lexing.from_channel (if !src = "" then stdin else open_in !src) in
        Parser.program Lexer.start lexbuf
    in
   
	if !pp then (KParseTreePrinter.print (read_program()))
	else if !psm5 then ignore (Sm5.print (Translator.trans (read_program())))
	else if !psonata then ignore (Sonata.print (Rozetta.trans (Translator.trans (read_program()))))
	else if !k then ignore (K.run (K.emptyMemory, K.emptyEnv, (read_program())))
	else if !sm5 then ignore (Sm5.run (Translator.trans (read_program())))
	else if !sonata then ignore (Sonata.run(Rozetta.trans (Translator.trans (read_program()))))
    else 
        (if !src <> "" then failwith "Specify one of the options if you want to read in .k- file" else
        let sm5_pgm = [PUSH (Val (Z 1)); PUSH (Val (Z 2)); ADD; PUT] in 
        ignore (Sonata.run(Rozetta.trans sm5_pgm))
        )

let _ = main ()
