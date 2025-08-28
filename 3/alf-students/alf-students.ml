
type student = {
  id: string;
  name: string;
  surname: string;
  vote: int option;
  laude: bool
}

let alf2023 = [
  { id="60/61/65570"; name="Ambra"; surname="Ambu"; vote=Some 21; laude=false };
  { id="61/61/65778"; name="Brunello"; surname="Brundu"; vote=Some 18; laude=false };
  { id="60/61/65624"; name="Costantino"; surname="Cossu"; vote=Some 24; laude=false };
  { id="60/61/65808"; name="Deborah"; surname="Demurtas"; vote=Some 28; laude=false };
  { id="60/61/65668"; name="Efisio"; surname="Ennas"; vote=Some 18; laude=false };
  { id="60/61/65564"; name="Felicino"; surname="Frau"; vote=None; laude=false };
  { id="60/64/20203"; name="Gavino"; surname="Girau"; vote=Some 20; laude=false };
  { id="60/61/65892"; name="heidi"; surname="hernandez"; vote=Some 8; laude=true };
  { id="60/61/65563"; name="Igino igor"; surname="Ibba"; vote=Some 15; laude=false };
  { id="60/61/64427"; name="Lillo"; surname="Lilliu"; vote=Some 25; laude=false };
  { id="60/61/65448"; name="Morgan"; surname="Murtas"; vote=Some 15; laude=false };
  { id="61/61/65213"; name="Nathan"; surname="Nieddu"; vote=Some 16; laude=false };
  { id="60/61/65832"; name="Ornella"; surname="Onnis"; vote=Some 30; laude=true };
  { id="60/61/65517"; name="Pinuccio"; surname="Puddu"; vote=Some 28; laude=false };
  { id="60/64/21222"; name="Quintilio"; surname="Quaglioni"; vote=Some 22; laude=false };
  { id="60/61/65907"; name="Rihanna"; surname="Ruzzu"; vote=Some 18; laude=false };
  { id="60/61/65766"; name="Samantah"; surname="Sulis"; vote=Some 30; laude=false };
  { id="60/61/65730"; name="Tatiana"; surname="Truzzu"; vote=Some 30; laude=true };
  { id="60/61/65738"; name="Ubaldo"; surname="Urru"; vote=None; laude=true };
  { id="60/61/65722"; name="Valentina"; surname="Vargiu"; vote=Some 30; laude=true };
  { id="60/61/65592"; name="Zlatan"; surname="Zuncheddu"; vote=Some 18; laude = false }
]


(*FILTRA LA LISTA E RENDE L'ID DI QUELLI CHE NON HANNO SOSTENUTO L'ESAME*)
let id_of_noshow lista_studenti = 
  lista_studenti
  |> List.filter (fun student -> student.vote = None)
  |> List.map (fun student -> student.id)
;;


let upgradeable lista_studenti = 
  lista_studenti
  |> List.filter (fun student -> 
      match student.vote with
      | Some v -> v >= 15 && v <= 17
      | _ -> false)
  |> List.map (fun student -> student.name ^" " ^ student.surname);;








  let f l = if l = [] then false else 
    let len = List.length l in
      let hd = List.hd l in
      if len >0 && hd = 0 || hd = 1 then 
        let rec check l=
      match l with 
      | []-> true
      | [x] -> x=2 || x=0 || x=1
      | h::t -> if h=0 || h=1 then check t else failwith "ciao" 
in check l
else false;;



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
  | (a, b) :: resto-> if (a>x && a>b) then a + (sum x lst) else if (b>a && b>x) then b + (sum x lst) else 0;;