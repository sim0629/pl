let val f = fn x => fn y => write (write x; y) in 
f  end
(* 'a -> 'b -> 'b *)
