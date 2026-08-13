let word = read_line ()
let check_lower_case c = Char.code c >= 97

let rec count_lower_cases word n =
  if n >= String.length word then 0
  else if check_lower_case word.[n] then 1 + count_lower_cases word (n + 1)
  else count_lower_cases word (n + 1)

let change_to_upp word =
  let l = count_lower_cases word 0 in
  let u = String.length word - l in
  if l >= u then false else true

let () =
  (* NOTE: use `lowercase` and `capitalize` when submitting to cf *)
  if change_to_upp word then print_string (String.uppercase_ascii word)
  else print_string (String.lowercase_ascii word)
