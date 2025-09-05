type player = Giallo | Rosso | Verde | Blu | Viola | Nero ;;
type territory = string ;;



let terrs : territory list =
[" Kamchatka "; " Jacuzia "];;


let adj : territory -> territory list = function
| " Urali " -> [" Afghanistan "; " Siberia "; " Cina "]
| " Jacuzia " -> [" Siberia "; " Kamchatka "; " Cita "]
| " Kamchatka " -> [" Jacuzia "; " Cita "; " Mongolia "; " Giappone "]
;;



type riskstate = ( string * player * int ) list ;;

let s0 : riskstate = [(" Jacuzia " , Verde , 2); (" Kamchatka " , Rosso , 3)];;


let enough_armies (s:riskstate) =
 s
 |> List.for_all (fun (_, _, armies) -> armies >= 1) 
;;


let rec has_territory (t:territory) (s:riskstate) =
  match s with 
  | [] -> false 
  | (a,_,_) :: r  -> if a = t then true else has_territory t r
;;


let no_dup (s: riskstate) : bool =
  let territories = List.map (fun (terr, _, _) -> terr) s in
  let unique_territories = List.sort_uniq String.compare territories in
  List.length territories = List.length unique_territories
;;

(*Quesa funzione fa lo stesso di no_dup

let rec no_dup (s:riskstate) = 
  match s with 
  |[] -> true 
  |(a,_,_) :: r -> if List.exists (fun (terr,_,_) -> terr = a) r then false 
  else (no_dup r)  
;;

*)


let  all_territories  (s:riskstate) =
 List.length s = List.length terrs
;;


(*Questa funzione in pratica cosa fa:
prende un territorio, un player, un numero di carroarmati e uno stato, confronta questi valori con un determinato stato
se il territorio è nuovo, quindi non era in possesso da nessun giocatore prima viene direttamente aggiunto allo stato aggioranto, 
se lo stato invece aveva già un proprietario controlla se il territorio all'interno dello stato è uguale a quello messo, se lo è aggiorna
la tupla con il nuovo giocatore e il numero di armate, e scorre tutta la lista, nel caso in cui non dovesse essere uguale allora lascia tutto invariato

*)
let rec update t p n s:riskstate =
  match s with 
  | [] -> [(t,p,n)]
  | (terr,p_vecchio,n_vecchio) :: rest -> if terr = t then (t,p,n)::rest 
  else (terr,p_vecchio,n_vecchio)::(update t p n rest)
;;


let rec get_armies (t:territory) (s:riskstate) =
  match s with
  |[] -> None
  |(terr,p,n)::rest -> if terr=t then Some (p,n) else get_armies t rest 
;;


let winner ((ta:territory),(va:int)) (td,vd) = if vd >= va then td else ta ;; 


(*let lost_armies (a,vala) (b,valb) = *)



