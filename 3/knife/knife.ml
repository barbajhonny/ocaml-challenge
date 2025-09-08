(*Knife*)
let f l = if List.length l > 1 then
  let testa = List.hd l in 
  let coda = List.rev l in
  if testa = List.hd coda then true else false
else false;;

let f (x:int) l = 
  l 
  |> List.for_all (fun n-> x < n )   
;;


let f l =
  match l with
  | 1 :: xs ->
      (match List.rev xs with
       | 1 :: rev_mid ->
           (* rev_mid è la parte centrale (in ordine invertito); la rimettiamo nell'ordine
              originale con List.rev e controlliamo che tutti gli elementi siano 0 *)
           List.for_all (fun z -> z = 0) (List.rev rev_mid)
       | _ -> false)
  | _ -> false
       ;;



let foo (n:int) l = 
  ((List.filter (fun x ->  x<= n ) l ), (List.filter(fun x ->  x>n ) l))

let f n l = l
    |> List.rev   
    |>List.fold_left (fun (minori,maggiori) x -> if x <= n then (x::minori,maggiori) else (minori, x::maggiori) ) ([],[])
;;


let rec f l1 l2 =
  match (l1,l2) with 
  | ([], []) -> []
  | (x :: xs,b :: bs)-> if b then x::f xs bs else f xs bs
  | _ -> failwith "ciao"
;;  

  
  let select (l1:int list) l2 =
  List.combine l1 l2
  |> List.filter_map (fun (x, b) -> if b then Some x else None);;


   let select (l1:int list) l2 =
  List.combine l1 l2
    |> List.filter (fun (_,x) -> x)
    |> List.map (fun (y,_)-> y)
  ;;



let f x l = 
  let occ = List.filter_map(fun y -> if y = x then Some y else None) l in
  if (List.length occ <= x) then true else false
;;


let foo n l = 
  let check = List.fold_left (fun acc x -> if x = n && acc<=n then acc+1 else acc ) 0 l in 
  if check <= n then true else false 
;;



let fuu l = 
  match  l with 
  |1::rest -> (match List.rev rest with
                  |1::rest -> List.for_all (fun x -> x=0)  rest
                  |_ -> false )
  |_-> false  
;;


let g l = 
  let head = List.hd l in
  let tail = List.hd (List.rev l) in
  
  if head = 1 && head = tail && List.length l >= 2 then 
    let f1 = List.filter (fun y -> y <>1) l in
    let f2 f1 = List.for_all (fun z -> z=0 || [z]=[]) f1 in 
    
      if (f2 f1) then true else false 
else false 
;;