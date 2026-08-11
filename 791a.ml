let split_on_char s =
  let sep = ' ' in
  let rec loop i last =
    if i = String.length s then [ int_of_string (String.sub s last (i - last)) ]
    else if s.[i] = sep then
      int_of_string (String.sub s last (i - last)) :: loop (i + 1) (i + 1)
    else loop (i + 1) last
  in
  if s = "" then [] else loop 0 0

let weights = read_line () |> split_on_char
let a, b = (List.nth weights 0, List.nth weights 1)

let rec count_years (a, b) year =
  if a > b then year else count_years (a * 3, b * 2) year + 1

let () = print_int (count_years (a, b) 0)
