(*funzione richieste nella prima parte d'esame*)
let ft= function
[]-> failwith "lista vuota"
| h::t -> h;;
(***************************)

let rec last= function
[]-> failwith "lista vuota"
 |[x]-> x
| h::t -> last t;;

(***************************)

let f l= if ft l= last l then true else false;;

(***************************)

let f x l=

(***************************)

let save_x = x in

(***************************)
(*per verificare se ci sono almeno x occorrenze all'interno della lista*)
let rec check x l =
  match l,x with
  |[x],0 -> true
  |[],0-> true
  |(h::t,x) when x>0 -> if save_x = h then check (x-1) t else check x t
  |_,_ -> false

  in check x l
;;

(***************************) 

(*Queste funzioni fanno la stessa cosa, prendono un intero e una lista di tuple, 
rendono la somma degli elementi delle tuple maggiori di x es f 3 [(4;2);(2,5)] = 9 (4+5)*)
let f x l = 
  let getValue = function
  | (v,c)  when x < v && v > c  -> v 
  | (v,c)  when x < c && v < c  -> c  
  | _ -> 0 in
  
  let lista_valori = List.map getValue l in

  let rec totale = function
    |[] -> 0
    |[x] -> x 
    |h::t -> h + (totale t)

  in totale lista_valori 
;; 


let rec sum x lst =
  match lst with
  | [] -> 0 
  | (a, b) :: resto-> if (a>x && a>b) then a + (sum x resto) else if (b>a && b>x) then b + (sum x resto) else 0
;;


(****************************************************)

(*VERIFICA UNA REGEX [01](00|2)?\1  ----IL PRIMO VALORE DEVE ESSERE 0 O 1, IL SECONDO PUO ESSERE 00 O 2 SE NO ANCHE NULLA
STRINGA ACCETTATA: [0]--[0;2]
STRINGA NON ACCETTATA [0;2;0]*)
  let f l = if l = [] then false else 
    let len = List.length l in
      let hd = List.hd l in
      if len >0 && hd = 0 || hd = 1 then 
        let rec check l=
      match l with 
      | []-> true
      | [x] -> x=2 || x=0 || x=1
      | h::t -> if h=0 || h=1 then check t else false
in check l
else false
;;
(****************************************************)
