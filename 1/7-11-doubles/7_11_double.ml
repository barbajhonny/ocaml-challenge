
let seven_eleven () = 
  let d1 = 1 + Random.int 6 in
  let d2 = 1 + Random.int 6 in
  let somma = d1+d2 in
  let b = if (somma = 7 || somma = 11 || d1 = d2) then true else false in
  (*qui rendo il valore b,d1,d2*)  
  (b,d1,d2)