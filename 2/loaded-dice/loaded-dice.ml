let dice p = 
  if  p>=0 && p<=100 then
  let random = Random.int 100 in
  if random <= p  then 6 else 1 + Random.int 5 
else failwith "numero negativo non ammesso";;

  let test () =
  for i = 1 to 10 do
    print_int (dice 80);
    print_string " "
  done;;