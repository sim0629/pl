(rec psfibo x => 
    ifzero (x.1 and 1) then x.2+0 
   else ( 
      ifzero ((x.1-1) and 2) then x.2+1 
      else ( 
         ifzero (0 and (psfibo (2,3))) 
         then ( psfibo(x.1-1,x.2) + psfibo(x.1-2,x.2) ) 
         else 0 (* this case will not be happend. *)
      ) 
   ) 
) (3, 2) 

(* psfibo is fibonacci seq. with some weight *) 
(* a0 = x.2, a1 = x.2+1, an = a(n-1) + a(n-2) *) 
(* if input (3,2), the result is 8, because a0=2, a1=3, a2=5, a3=8. *)
