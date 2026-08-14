let num = read_int ()

let rec exist i l =
  match l with [] -> false | h :: t -> if h = i then true else exist i t

let rec all_lucky_nums num =
  let check_list = [ 4; 7 ] in
  if num / 10 = 0 then exist num check_list
  else if exist (num mod 10) check_list then all_lucky_nums (num / 10)
  else false

let rec check_almost_lucky num i =
  if i > num then false
  else if num mod i = 0 && all_lucky_nums i then true
  else check_almost_lucky num (i + 1)

let () =
  if check_almost_lucky num 1 then print_string "YES" else print_string "NO"
