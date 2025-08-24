type card = Joker | Val of int 

let win gioc mazz  =
match (gioc,mazz) with
| Joker , Joker -> (false) 
| Joker, _ -> (true)
| _ , Joker -> (false)
| Val g, Val m -> g > m  
