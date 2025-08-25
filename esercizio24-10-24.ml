(*funzione richieste nella prima parte d'esame*)
let ft= function
[]-> failwith "lista vuota"
| h::t -> h;;

let rec last= function
[]-> failwith "lista vuota"
 |[x]-> x
| h::t -> last t;;


let f l= if ft l= last l then true else false;;



