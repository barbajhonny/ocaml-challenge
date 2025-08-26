Random.self_init ();;

type suit = S | H | D | C ;;
type card = Card of int * suit ;;

let suitRnd () =
  match 1+Random.int 4 with
  |1 -> S
  |2 -> H
  |3 -> D
  |_ -> C
;;

let valueRnd ()= 1 + Random.int 13;;

let rndHand () =
  (Card(valueRnd(),suitRnd()) ,Card(valueRnd(),suitRnd()) ,Card(valueRnd(),suitRnd()) ,Card(valueRnd(),suitRnd()),Card(valueRnd(),suitRnd()))
;;

(*let poker (c1,c2,c3,c4,c5) = 
let valore (Card(a,_)) = a in
  let lista  = List.sort compare[valore(c1);valore(c2);valore(c3);valore(c4);valore(c5)] in

  let rec equal lista count= 
  match (lista,count) with
  | ([],p) when p<4 -> false
  | (h::x::t,p) -> if h=x then equal t (count+2) else equal t (count)  

in equal lista 0
;;
*)