let  square x = x * x


let rec potenza x n =
  if n = 0 then 1           (* caso base: x^0 = 1 *)
  else x * potenza x (n - 1)


let exp9 x = potenza x 9
  