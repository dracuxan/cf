let p = read_int ()

let rec count_moves p max =
  if p < max then count_moves p (max - 1)
  else if p - max = 0 then 1
  else 1 + count_moves (p - max) max

let () = Printf.printf "%d" (count_moves p 5)
