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

let rec no_dup (s:riskstate) = 
  match s with 
  |[] -> true 
  |(a,_,_) :: r -> if List.exists (fun (terr,_,_) -> terr = a) r then false 
  else (no_dup r)  
;;


let  all_territories  (s:riskstate) =
 List.length s = List.length terrs

;;