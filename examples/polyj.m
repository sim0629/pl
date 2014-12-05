let val comp = fn x=> fn y=> (x = y) in 
   comp 3 1; 
   comp "asdf" "gg"; 
   comp (fn x=>1) (fn y=>1) 
end
(* error (function is not comparable) *)
