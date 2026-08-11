let split_on_char s =
  let sep = ' ' in
  let rec loop i last =
    if i = String.length s then [ int_of_string (String.sub s last (i - last)) ]
    else if s.[i] = sep then
      int_of_string (String.sub s last (i - last)) :: loop (i + 1) (i + 1)
    else loop (i + 1) last
  in
  if s = "" then [] else loop 0 0

let line = read_line () |> split_on_char
let m, n, a = (List.nth line 0, List.nth line 1, List.nth line 2)

let calc_req_tiles s t =
  Int64.div (Int64.sub (Int64.add s t) (Int64.of_int 1)) t
(* if s mod t = 0 then s / t else (s / t) + 1 *)

let () =
  let side_a = calc_req_tiles (Int64.of_int m) (Int64.of_int a) in
  let side_b = calc_req_tiles (Int64.of_int n) (Int64.of_int a) in
  Printf.printf "%Ld\n" (Int64.mul side_a side_b)
