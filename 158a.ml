let l1 = read_line ()
let l2 = read_line ()

let split_on_char sep s =
  let rec loop i last =
    if i = String.length s then [ int_of_string (String.sub s last (i - last)) ]
    else if s.[i] = sep then
      int_of_string (String.sub s last (i - last)) :: loop (i + 1) (i + 1)
    else loop (i + 1) last
  in
  if s = "" then [] else loop 0 0

let () =
  let kth =
    let l = split_on_char ' ' l1 in
    List.nth l 1
  in

  let scores = split_on_char ' ' l2 in
  let min_score = List.nth scores (kth - 1) in

  let rec calc_selected l acc =
    match l with
    | [] -> acc
    | i :: t ->
        if i >= min_score && i > 0 then calc_selected t acc + 1
        else calc_selected t acc
  in
  Printf.printf "%d" (calc_selected scores 0)
