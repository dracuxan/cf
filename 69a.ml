let split_on_char s =
  let sep = ' ' in
  let rec loop i last =
    if i = String.length s then [ int_of_string (String.sub s last (i - last)) ]
    else if s.[i] = sep then
      int_of_string (String.sub s last (i - last)) :: loop (i + 1) (i + 1)
    else loop (i + 1) last
  in
  if s = "" then [] else loop 0 0

let rec sum_l = function [] -> 0 | h :: t -> h + sum_l t
let n = read_int ()

let () =
  let sum_x = ref 0 in
  let sum_y = ref 0 in
  let sum_z = ref 0 in
  for i = 1 to n do
    let l = read_line () |> split_on_char in
    sum_x := List.nth l 0 + !sum_x;
    sum_y := List.nth l 1 + !sum_y;
    sum_z := List.nth l 2 + !sum_z
  done;
  if !sum_x = 0 && !sum_z = 0 && !sum_y = 0 then print_string "YES"
  else print_string "NO"
