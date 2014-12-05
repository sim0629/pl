(* example 1: polymorphic toys *) 
let val I = fn x => x 
val add = fn x => x.1 + x.1 
val const = fn n => 10 
in 
I I; 
add(1, true) + add(2, "snu 310 fall 2009"); 
const 1 + const true + const "kwangkeun yi" 
end 
(* int *)
