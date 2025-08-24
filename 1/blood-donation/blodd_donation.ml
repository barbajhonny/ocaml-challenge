
type blood_group = A | B | AB | O
let check_groups a b = 
  match a with
   | O -> true
   | A -> b = A || b = AB
   | B -> b = B || b = AB
   | AB -> b = AB
   
  (*O può donare a tutti
  A può donare solo a se stesso e ad AB
  B puo donare solo a se stesso e ad AB
  AB può donare solo a se stesso*)

  (* Test con assert *)
let () =
  assert (check_groups O O = true);
  assert (check_groups O A = true);
  assert (check_groups A B = false);  (* A non può donare a B *)
  assert (check_groups AB AB = true);
  assert (check_groups B O = false);
  print_endline "Tutti i test sono passati!"
