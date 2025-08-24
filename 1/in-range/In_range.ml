let in_range x a b =
  let min  = if x>= a then true else false in
  let max  = if x<= b then true else false in 
  (min && max)