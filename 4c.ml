let n = read_int ()

let rec exist name_arr name =
  let len = Array.length name_arr in
  let rec loop i =
    if i >= len then 0
    else if name_arr.(i) = name then 1 + loop (i + 1)
    else loop (i + 1)
  in
  loop 0

let () =
  let name_arr = Array.make n "" in
  for i = 0 to n - 1 do
    let curr = read_line () in
    let occ = exist name_arr curr in
    if occ > 0 then Printf.printf "%s\n" (curr ^ string_of_int occ)
    else Printf.printf "OK\n";
    name_arr.(i) <- curr
  done
