(*MALLOC, BANG *) 
let val x = (malloc 3, malloc 4) in 
x.1 := read; x.2 := read; 
write (!x.1 + !x.2) 
end 

(* result : input 10 20 => output 30 *)
