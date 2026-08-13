let split_on_char s =
  let sep = ' ' in
  let rec loop i last =
    if i = String.length s then [ int_of_string (String.sub s last (i - last)) ]
    else if s.[i] = sep then
      int_of_string (String.sub s last (i - last)) :: loop (i + 1) (i + 1)
    else loop (i + 1) last
  in
  if s = "" then [] else loop 0 0

let numbers = read_line () |> split_on_char
let n, k = (List.nth numbers 0, List.nth numbers 1)

let rec cal_result n k =
  if k <= 0 then n
  else if n mod 10 = 0 then cal_result (n / 10) (k - 1)
  else cal_result (n - 1) (k - 1)

let () = print_int (cal_result n k)
