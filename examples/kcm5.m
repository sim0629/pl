let rec f = fn x => f x in
  (fn a =>
    1 + (f 1);
    "a" = (f 3);
    f "4"
  )
end
