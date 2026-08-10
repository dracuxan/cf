let split_string word =
  let rec loop i acc = if i < 0 then acc else loop (i - 1) (word.[i] :: acc) in
  loop (String.length word - 1) []

let rec count_instances c word acc =
  match word with
  | [] -> acc
  | head :: tail ->
      if head = c then count_instances c tail acc + 1
      else count_instances c tail acc

let is_even num = num mod 2 = 0

let rec num_distninct_chars word acc =
  match word with
  | [] -> acc
  | h :: t ->
      let inst = if count_instances h word 0 > 1 then true else false in
      if inst then num_distninct_chars t acc else num_distninct_chars t acc + 1

let check word =
  if is_even (num_distninct_chars word 0) then "CHAT WITH HER!\n"
  else "IGNORE HIM!\n"

let name = read_line () |> split_string
let () = print_string (check name)
