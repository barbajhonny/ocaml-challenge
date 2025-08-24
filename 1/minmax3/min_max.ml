(*PRIMA SOLUZIONE:
let minmax3 a b c = 
  let min  = if a<b  && b<c then a else if b<a && b<c then b else c in
  let max  = if a>b && a > c then a else if b>a && b>c then b else c in 
  (min, max)
  *)

  let minmax3 a b c = 
    let min = min (min a b) c in
    let max = max ( max a b ) c in
    (min,max)