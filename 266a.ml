let stones = read_int ()
let order = read_line ()

let rec remove_stones order n =
  if n >= String.length order - 1 then 0
  else if order.[n] = order.[n + 1] then 1 + remove_stones order (n + 1)
  else remove_stones order (n + 1)

let () = print_int (remove_stones order 0)
