let val g = fn t => let rec f = fn x=> f (write x) in f t end in g (malloc 1) end
(* error (write location ?) *)
