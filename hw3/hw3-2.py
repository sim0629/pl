money = [1, 10, 100, 500, 1000, 5000, 10000, 50000]

def numch_in(i, n):
  if i < 1:
    return 1
  else:
    s = numch_in(i - 1, n)
    money_i = money[i]
    while money_i < n + 1:
      n -= money_i
      s += numch_in(i - 1, n)
    return s

def numch(n):
  return numch_in(7, n)

print numch(10000)
