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
    |> List.map (fun (y,_)-> y);;