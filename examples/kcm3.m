let val f = fn x => fn y => x = y in
  write (f 1 1);
  f "a" "b"
end
