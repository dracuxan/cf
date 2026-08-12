let split_on_char s =
  let sep = ' ' in
  let rec loop i last =
    if i = String.length s then [ int_of_string (String.sub s last (i - last)) ]
    else if s.[i] = sep then
      int_of_string (String.sub s last (i - last)) :: loop (i + 1) (i + 1)
    else loop (i + 1) last
  in
  if s = "" then [] else loop 0 0

let ints = read_line () |> String.split_on_char ' '

let k, n, w =
  ( int_of_string (List.nth ints 0),
    int_of_string (List.nth ints 1),
    int_of_string (List.nth ints 2) )

let rec sum n = if n = 1 then 1 else n + sum (n - 1)
let price_of_bananas = k * sum w
let money_to_borrow = if price_of_bananas > n then price_of_bananas - n else 0
let () = print_int money_to_borrow
