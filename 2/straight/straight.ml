Random.self_init ();;

type suit = S | H | D | C
type card = Card of int * suit;;

let randomSuit ()=
  match 1+Random.int 4 with
  |0->S
  |1->H
  |2->D
  |_->C ;;

let randomNumber () = 1 + Random.int 13;;

let carta () = Card(randomNumber(),randomSuit());;

let mano () = (carta(),carta(),carta(),carta(),carta());;

let carte1 = mano () 

(*FUNZIONE FINALE*)
let straight (c1,c2,c3,c4,c5) = 
  let carte = [c1;c2;c3;c4;c5] in 

  let getValue =  function
  |Card(v,_) -> v in

  let lista_valori = List.map getValue carte in 
  let lista_ordinata = List.sort compare lista_valori in

    match lista_ordinata with
    | [a;b;c;d;e]->  b = a+1 && c= b+1 && d=c+1 && e=d+1  
    | _ -> false
  ;;