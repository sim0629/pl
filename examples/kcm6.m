let rec f = fn x => f x in
  let val a = f 1 in
    a
  end
end
