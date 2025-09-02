
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

(*FILTRA LA LISTA E RENDE IL NOME E IL COGNOME DEGLI STUDENTI CHE HANNO PRESO UN VOTO COMPRESO TRA 15 E 17*)
let upgradeable lista_studenti = 
  lista_studenti
  |> List.filter (fun student -> 
      match student.vote with
      | Some v -> v >= 15 && v <= 17
      | _ -> false)
  |> List.map (fun student -> student.name ^" " ^ student.surname)
;;


(*FILTRA LA LISTA E CAMBIA IL VOTO IN 18 A CHI HA PRESO UN VOTO COMPRESO TRA 15 E 17*)
let upgrade lista_studenti=
  lista_studenti
  |> List.filter (fun student -> 
      match student.vote with
      | Some v -> v >= 15 && v <= 17
      | _ -> false)
  |> List.map (fun student -> {student with vote = Some 18})

;;


(*FILTRA LA LISTA E RENDE IL NOME E IL COGNOME DEGLI STUDENTI CHE HANNO UAN LODE MA, UN VOTO MINORE DI 30 O NON HANNO PROPRIO VOTO*)
let wrong_laude lista_studenti =
  lista_studenti
  |> List.filter (fun student -> match  (student.vote , student.laude) with
    | (Some x, y) when x < 30  && y -> true 
    | (x, y) when  x =None  && y -> true 
    | (_,_) -> false) 
  |> List.map (fun student -> student.name ^ " " ^student.surname)
;;


(*FILTRA LA LISTA E TOGLIE LA LODE A CHI NON HAPRESO ALMENO 30 O NON HA FATTO L'ESAME*)
let fix_laude lista_studenti = 
  lista_studenti 
    |> List.filter (fun student-> match (student.vote,student.laude) with
      |(Some x,true) when x<30 -> true
      |(None,true) -> true
      |(_,_) -> false )
    |>List.map (fun student -> {student with laude = false} )
;;
   

(*FILTRA LA LISTA E RENDE LA PERCENTUIALE DI STUDENTI CHE HANNO PASSATO L'ESAME*)
let percent_passed lista_studenti =
  let total = List.length lista_studenti in
  let passed =
    lista_studenti
      |> List.filter (fun student -> match student.vote with
        | Some x when x>= 18 -> true
        | _ -> false )
      |> List.length in
    if total > 0 then ((float_of_int passed /. float_of_int total) *. 100.0)
      else 
      0.0
;;


(*FILTRA LA LISTA E RENDE LA MEDIA DEI VOTI DEGLI STUDENTI CHE HANNO PASSATO L'ESAME*)
let avg_vote lista_studenti =
  let votes_with_bonus =
    lista_studenti
    |> List.filter_map (fun student -> 
        match student.vote, student.laude with
        | (Some v, _) when v >= 18 -> 
            let bonus = if student.laude then 2.0 else 0.0 in
            Some (float_of_int v +. bonus)
        | _ -> None)
  in
  if votes_with_bonus = [] then 0.0
  else 
    let total = List.fold_left (+.) 0.0 votes_with_bonus in
    total /. float_of_int (List.length votes_with_bonus)
    






    