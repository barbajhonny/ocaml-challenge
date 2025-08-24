Random.self_init ();;

let guess5 n =
  if n <=0 || n>5 then (false,-1) 
  else let r = 1 + Random.int(6) in
  if r = n then (true,r) else (false,r) 

