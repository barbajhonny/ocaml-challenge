type winner = Player | Computer | Tie;;

let win (p_n,p_guess) =
  if p_n>= 0 && p_n<=5 && p_guess>=0 && p_guess<=10 then
    let c_random_number = Random.int 6 in
    let c_random_guess =  Random.int 11 in
    if p_n+c_random_number = p_guess then ((c_random_number,c_random_guess),Player) 
    else if p_n+c_random_number = c_random_guess then ((c_random_number,c_random_guess),Computer)
    else ((c_random_number,c_random_guess),Tie) 
  else failwith "out of range";;