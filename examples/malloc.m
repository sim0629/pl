let val f = malloc (fn x => x) in
  f := fn x => x + 1;
  (!f) true
end
