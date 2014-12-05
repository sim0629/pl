let rec f = fn x=>x in 
f "이것은"; f true; f "폴리모픽"; f "identity"; f ("function", 1); f (malloc false) 
end
(* loc(bool) *)
