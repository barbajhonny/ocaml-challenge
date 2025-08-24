(*  PRIMA SOLUZIONE SENZA L'INTERVALLO
let movie_rating x y z =
  match (x,y,z) with
  |(x,y,z) when x+y+z = 15-> "MASTERPICE"
  |(x,y,z) when x+y+z = 14 -> " ALTAMENTE CONSIGLIATO"
  |(x,y,z) when x+y+z <14 && x+y+z >= 11 -> "CONSIGLIATO"
  | _  -> "RECENSIONI MISTE"
*)

let always_fail x = failwith("this function always fails")

let  movie_rating x y z =
match (x,y,z) with
|(x,y,z) when (x>= 1 && x<=5) && (y>= 1 && y<=5) && (z>= 1 && z<=5) -> 
  let sum = x+y+z in
  if sum = 15 then "MASTERPICE"
  else if sum = 14 then "ALTAMENTE CONSIGLIATO"
  else if sum < 14 && sum >= 11 then "CONSIGLIATO"
  else "VALORI MISTI"
  |_ -> always_fail x

  