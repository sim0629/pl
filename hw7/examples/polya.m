let rec f = fn x=> let rec g = fn x => f x in g x end in f end
(* 'a -> 'b *)
