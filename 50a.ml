let l = read_line ()

let split_on_char s =
  let sep = ' ' in
  let rec loop i last =
    if i = String.length s then [ int_of_string (String.sub s last (i - last)) ]
    else if s.[i] = sep then
      int_of_string (String.sub s last (i - last)) :: loop (i + 1) (i + 1)
    else loop (i + 1) last
  in
  if s = "" then [] else loop 0 0

let m, n =
  let l_list = split_on_char l in
  (List.nth l_list 0, List.nth l_list 1)

let () =
  let answer = m * n / 2 in
  Printf.printf "%d" answer
