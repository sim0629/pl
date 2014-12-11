(fn even =>
  (rec sum list =>
    ifzero (list.1 - 1)
    then 0
    else (
      ifzero (even (list.1))
      then (sum (list.2))
      else ((list.1) + (sum (list.2)))
    )
  ) (
    (fn n =>
      (rec fibo list =>
        (fn v =>
          ifzero (n - v)
          then list
          else (fibo (v, list))
        ) ((list.1) + (list.2.1))
      ) (2, (1, 0))
    ) 10
  )
) (rec even n =>
  ifzero n
  then 1
  else (
    ifzero (n - 1)
    then 0
    else (even (n - 2))
  )
)
