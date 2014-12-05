let rec f = fn x => (f "wook" ; f x) in (f 1) end
(* ERROR (string, int unification fail) *)
