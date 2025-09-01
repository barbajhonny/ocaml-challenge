(*funzione richieste nella prima parte d'esame*)
(*let ft= function
[]-> failwith "lista vuota"
| h::t -> h;;

let rec last= function
[]-> failwith "lista vuota"
 |[x]-> x
| h::t -> last t;;


let f l= if ft l= last l then true else false;;



let f x l=

let save_x = x in

(*per verificare se ci sono almeno x occorrenze all'interno della lista*)
let rec check x l =
  match l,x with
  |[x],0 -> true
  |[],0-> true
  |(h::t,x) when x>0 -> if save_x = h then check (x-1) t else check x t
  |_,_ -> false

  in check x l ;;*)


























