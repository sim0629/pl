let val f = fn x => fn y => write (write x; y) in 
f (0, 0) 
end
(* error ( pair 는 write 불가) *)
