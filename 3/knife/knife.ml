(*Knife*)

(* Rende un booleano e controlla se in una lista la testa, coincide con l'ultimo elemento *)
let f l = if List.length l > 1 then
  let testa = List.hd l in 
  let coda = List.rev l in
  if testa = List.hd coda then true else false
else false
;;
(* Rende un booleano e controlla se in una lista la testa, coincide con l'ultimo elemento *)
let f l = 
  l
    |> fun x-> if List.hd x = List.hd (List.rev x) then true else false 
;; 


(* Verifica se all'interno di una lista tutti gli elementi della lista siano piu grandi di x *)
let f (x:int) l = 
  l 
    |> List.for_all (fun n-> x < n )   
;;


(* Questo è un riconoscitore di linguaggio, la parola accettata da true se e solo se inizia con 1, 
  al suo interno ci son solo 0 e termina con 1, oppure inizia e termina con 1 *)
let f l =
  match l with
  | 1 :: xs ->
      (match List.rev xs with
       | 1 :: rev_mid ->
           (* rev_mid è la parte centrale (in ordine invertito);
              controlliamo che tutti gli elementi siano 0 *)
           List.for_all (fun z -> z = 0) (rev_mid)
       | _ -> false)
  | _ -> false
;;


(* Questa funzione prende un intero e una lista, mette a destra di una tupla i numeri della lista minori o uguali all'intero passato, 
  mentre a sinistra mette i valori della lista più grandi di n *)
let foo (n:int) l = 
  l |>
  fun z->((List.filter (fun x ->  x<= n ) z ), (List.filter(fun x ->  x>n ) z))
;;
(* Questa funzione prende un intero e una lista, mette a destra di una tupla i numeri della lista minori o uguali all'intero passato, 
  mentre a sinistra mette i valori della lista più grandi di n *)
let f n l = l
    |> List.rev   
    |>List.fold_left (fun (minori,maggiori) x -> if x <= n then (x::minori,maggiori) else (minori, x::maggiori) ) ([],[])
;;


(* Questa funzine prende una lista qualsiasi e la confronta con la seconda lista di booleani,
  se nella lista l2 compare un true allora rende il valore della lista l1 *)
let rec f l1 l2 =
  match (l1,l2) with 
  | ([], []) -> []
  | (x :: xs,b :: bs)-> if b then x::f xs bs else f xs bs
  | _ -> failwith "ciao"
;;  

(* Questa funzine prende una lista qualsiasi e la confronta con la seconda lista di booleani,
  se nella lista l2 compare un true allora rende il valore della lista l1 *)
let select (l1:int list) l2 =
  (*combina due liste e le trasforma in una lista di tuple assocciando il  primo valore della prima lista con il primo valore della seconda lista*)
  List.combine l1 l2 

  (*questa funzione serve per filtrare e rendere direttamente un valore
  NOTA : NON rende un 'a option ma un int (anche se c'è scritto Some) in caso di None, rende*)
  |> List.filter_map (fun (x, b) -> if b then Some x else None) 
;;


(* Questa funzine prende una lista qualsiasi e la confronta con la seconda lista di booleani,
  se nella lista l2 compare un true allora rende il valore della lista l1 *)
let select (l1:int list) l2 =
  List.combine l1 l2
    |> List.filter (fun (_,x) -> x) (* Qui filtra solo i valori che contengono TRUE*)  
    |> List.map (fun (y,_)-> y) (* Qui rende solo i valori interi che prima son stati filtrati con TRUE , risultato? = solo i numeri con true*)
;;


(* Questa funzione rende true se ci sono al più x occorrenze di x all'interno di una lista *)
let f x l = 
  let occ = List.filter_map(fun y -> if y = x then Some y else None) l in   (*Filtra e rende un intero*)
  if (List.length occ <= x) then true else false 
;;
(* Uguale *)
let foo n l = 
  let check = List.fold_left (fun acc x -> if x = n && acc<=n then acc+1 else acc ) 0 l in 
  if check <= n then true else false 
;;


(* Matcha il linguaggio { 1 0^n 1 -> n>=0} *)
let fuu l = 
  match  l with 
  |1::rest -> (match List.rev rest with
                  |1::rest -> List.for_all (fun x -> x=0)  rest
                  |_ -> false )
  |_-> false  
;;
(* Uguale *)
let g l = 
  let head = List.hd l in
  let tail = List.hd (List.rev l) in
  
  if head = 1 && head = tail && List.length l >= 2 then 
    let f1 = List.filter (fun y -> y <>1) l in
    let f2 f1 = List.for_all (fun z -> z=0 || [z]=[]) f1 in 
    
      if (f2 f1) then true else false 
else false 
;;












type t = X | O | P ;;
let  win l =
  if List.length l > 3 then
    let rec check l=
  match l with 
  |X::x::t when x=X-> if List.hd (x::t) = List.hd t then Some X else check (x::t) 
  |O::x::t when x=O-> if List.hd (x::t) = List.hd t then Some O else check (x::t)
  |_ -> None
  in check l 
  else None;;
;;








let f n l= 
 List.filter_map (fun (x,y)-> if y>n then Some x else None) l 
|> function
  |[]-> None
  |h::t -> Some(List.fold_left (fun acc x-> if x<acc then x else acc) h t)
;;



(*let rec f n l= 

let rec minlst l = match l with 
|[]->failwith "impossibile"
|(x,y)::rest-> if y>n then x::minlst rest else minlst rest 
in

let rec minimo minlst = 
  match minlst with
  |[]-> None
  |[x]-> (Some x)
  |h::t-> let tail = minimo t in 
      if h<tail then h else tail in

match minlst l with 
|[]->None
|x -> Some(x)
;;*)


(*************************************************)

let rec f n l =
  match l with
  | [] -> None
  | (x, y) :: rest ->
      let rest_result = f n rest in
      if y > n then
        match rest_result with
        | None -> Some x
        | Some m -> if x < m then Some x else Some m
      else
        rest_result
;;

(********************************************************)

let rec f n l =
  match l with
  | [] -> None  (* Caso base: lista vuota *)
  | (x, y) :: rest ->
      match f n rest with  (* Chiamata ricorsiva *)
      | None -> 
          if y > n then Some x else None
      | Some min_rest ->
          if y > n then
            if x < min_rest then Some x else Some min_rest
          else
            Some min_rest
;;




let disj (a,b) (c,d) = 
  if a<b && b<c && c<d then true else false
;;


let disj (a,b) (c,d) =
  match (a,b) , (c,d) with 
  |(a,b) , (c,d) -> if b>=c || (d>=a && d<=b) then false else true
;; 

let incl (a,b) (c,d)= 
  if a>=c && b<d then true else false
;;

let rec find (a,b) l =
  match (a,b),l with  
  |(a,b) , h::t -> if disj (a,b) h then find (a,b) t else Some(h)
  |_ -> None
;;


let rec rmdup l = 
  match l with 
  |h :: x :: t -> if incl h x then x::t 
                  else if incl x h then h::t 
                    else rmdup (h::t)
  |_ -> l
;;


(*cat | grep -E '^([0-1]{1,2})[01]{,1}\1$'*)


 let pi ingr prezzi = 
  prezzi 
|> List.fold_left(fun acc (x,y)-> if x=ingr then acc+y else acc) 0
|>function
|x when x>0 -> x
|_ ->failwith"ciao"
;;


let rec pil ingrl prezzi=
match ingrl with
| x::z ->  ((pi x prezzi) + pil z prezzi)
|[] -> 0
;;



let rec costo p menu prezzi = 
  match menu with
  |(x,y)::t-> if p=x then 300+(pil y prezzi) else (costo p t prezzi)
  |[]->0
;;
