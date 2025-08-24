let f1 x = x=2;;


let f2 t = if t then 2 else 4;;


let f3 x = if x=2 then (3,true) else (4,false);;


let f4 (x,true) = if x>0 then 1 else 0;;


(*questa dovrebbe essere una funzione che prende un intero e restituisce una funzione che a sua volta
retituisce un intero, quindi cosa succede? si crea la funzine f5 che prende come input un intero e poi 
a quell'intero viene sommato un altro intero n come possiamo vedere ora mi avvalgo della funzione aggiungi5 per aggiungere
ad ogni x 5,quindi se metto 3 mi rende 8, se metto 6 mi rende 11....*)
let f5 x = fun n-> n+x;;
let aggiungi5 = f5 5;;


let f6 x n = match x>0 with
| true -> if x = n then true else failwith "il numero è differente"
| false-> failwith "il numero deve essere maggiore di 0";;  


let f7 x n = match x with
| true -> if n>0 then true else failwith "il numero deve essere positivo" 
| false-> if n<0 then false else failwith "il numero deve essere negativo";;  
 

let f8 x = if x then (fun n -> if n then 1 else 0) else failwith "il numero deve essere positivo" ;; 


let f9 x = if x then (fun n -> if n>0 then 1 else 0) else failwith "il numero deve essere positivo" ;; 


let raddoppia = fun x -> x*2;;
let f10 raddoppia = 
  let ris = raddoppia (-3) in
  if ris>0 then ris else failwith "numero negativo";;
;;




let positive = fun x  -> if x>0 then true else false;;
let f11 positive =
  let ris = positive 3 in
if ris then true else failwith "numero negativo";;
;;


let f12 f = f true + f false;; 
let f1321 f = if f(true)>0 then 2 else 4;;


let f13 f = if f(1) then true else false;;


let f14 f = if f(true) then 2 else 3;;


let f15 x =fun (e,y) -> if (e+2,y+2)=(2,2) then x+2 else 3;;


let f16 x = 
  fun b c-> x+b+c;;


let f17 f = fun x-> if f(2)>0 then f(f x) else failwith "ciao"  ;;


let f18 f = if f(fun x->x+2)>0 then 1 else 0;; 


let f19 f = if f(2)>0  then fun x-> x=true else failwith "ciao" ;;


let f19 f = if f(true) then fun x-> x+1 else failwith "ciao" ;;



    





