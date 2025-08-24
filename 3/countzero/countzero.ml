let countzero f a b =
  let rec count current total =
    if current > b then total
    else if f current = 0 then count (current + 1) (total + 1)
    else count (current + 1) total
  in
  count a 0;;


assert (countzero (fun x -> x) (-10) 10 = 1);;

assert (countzero (fun x -> x) 1 10 = 0);;

assert (countzero (fun x -> x*x - 1) (-10) 10 = 2);;

assert (countzero (fun x -> (if x<0 then -x else x) - 1) (-10) 10 = 2);;
