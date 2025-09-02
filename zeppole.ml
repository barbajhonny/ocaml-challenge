type tingr = Farina | Latte | Uova

type tstate = { 
  disp : tingr -> int ;
  zepp : int ;   
  balance : int
}

let s0 ={ disp =( function Farina -> 1 | Latte -> 0 | Uova -> 0); zepp =0; balance =1000 };;

let is_valid s =
  match s with
  | { disp = d; zepp = z; balance = b } ->
    d Farina > 0 && d Latte > 0 && d Uova > 0 && z > 0 && b > 0
;;

let price ingr = match ingr with 
| f when f = Farina-> 10 
| l when l = Latte-> 20 
| u when u = Uova-> 25 
|_-> 0
;;


let add_ingr s (ingr, n) =
  match ingr with
  | Farina -> { s with disp = fun x -> if x = Farina then s.disp x + n else s.disp x }
  | Latte -> { s with disp = fun x -> if x = Latte then s.disp x + n else s.disp x }
  | Uova -> { s with disp = fun x -> if x = Uova then s.disp x + n else s.disp x }
;;

let nuovo_stato = add_ingr s0 (Farina, 2);;

let buy (ing, n) s =
  let cost = n * price ing in
  if s.balance >= cost then
    { (add_ingr s (ing, n)) with balance = s.balance - cost }
  else
s;;


let s1 =  { disp =( function Farina -> 20 | Latte -> 20 | Uova -> 10); zepp =0; balance =150 }

let cook (s:tstate) = if s1.disp Farina >= 5 && s1.disp Latte >= 2 && s1.disp Uova >= 3 then 
{s1 with disp= (function
  |Farina -> s1.disp Farina-5
  |Latte -> s1.disp Latte-2
  |Uova -> s1.disp Uova-3);
  zepp = s1.zepp+40}
else s1
;;

(*Dopo la funzione cook Farina = 15, Latte = 18, Uova = 7, zeppole = 40, balance = invariato*)


let sell (n:int) s = if s.zepp > 0 && n <= s.zepp && n > 0 then
  {s with 
  zepp = s.zepp-n;
  balance = s.balance + (n*50)}
else failwith "non ci sono abbastanza zeppole"
;; 
  

type label = Buy of tingr * int | Cook | Sell of int;;

let step l s =
  match l with
  | Buy (ingr, n) -> buy (ingr, n) s  (* buy già fa il controllo del balance *)
  | Cook -> cook s                    (* cook già fa il controllo ingredienti *)
  | Sell n -> sell n s                (* sell già fa il controllo zeppole *)
;;