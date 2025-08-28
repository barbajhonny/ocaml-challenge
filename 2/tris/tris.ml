let tris (a,b,c,d) = match (a,b,c,d) with  
| ( a , b , c , _ ) when a=b && b=c -> true
| ( a , _ , c , d ) when a=c && c=d -> true
| ( a , b , _ , d ) when a=b && b=d -> true
| ( _ , b , c , d ) when b=c && c=d -> true
| _ -> false 

let rndNumber () = 1+Random.int 10

let hand () = ((rndNumber()),(rndNumber()),(rndNumber()),(rndNumber()));;


