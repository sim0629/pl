let val f = fn x => write x in 
f 1; f true; f "asdf"; f (1,2) 
end
(* error ( pair is not able to be written ) *)
