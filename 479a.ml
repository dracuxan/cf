let a = read_int ()
let b = read_int ()
let c = read_int ()

let find_max a b c =
  let comb1 = (a + b) * c in
  let comb2 = a + (b * c) in
  let comb3 = a * (b + c) in
  let comb4 = (a * b) + c in
  let comb5 = a * b * c in
  let comb6 = a + b + c in
  max (max (max comb1 comb2) (max comb3 comb4)) (max comb5 comb6)

let () = print_int (find_max a b c)
