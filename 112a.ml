let lowercase_ascii ch =
  if ch >= 'A' && ch <= 'Z' then Char.chr (Char.code ch + 32) else ch

let convert_to_num c = lowercase_ascii c |> Char.code

let split_string word =
  let rec loop i acc = if i < 0 then acc else loop (i - 1) (word.[i] :: acc) in
  loop (String.length word - 1) []

let rec compare_list l1 l2 =
  match (l1, l2) with
  | [], [] -> 0
  | h1 :: t1, h2 :: t2 ->
      if convert_to_num h1 > convert_to_num h2 then 1
      else if convert_to_num h1 < convert_to_num h2 then -1
      else compare_list t1 t2
  | _ -> 0

let l1 = read_line () |> split_string
let l2 = read_line () |> split_string

let () =
  let answer = compare_list l1 l2 in
  print_int answer
