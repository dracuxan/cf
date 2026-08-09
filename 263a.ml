let matrix = Array.make_matrix 5 5 0

let split_on_char s =
  let sep = ' ' in
  let rec loop i last =
    if i = String.length s then [ int_of_string (String.sub s last (i - last)) ]
    else if s.[i] = sep then
      int_of_string (String.sub s last (i - last)) :: loop (i + 1) (i + 1)
    else loop (i + 1) last
  in
  if s = "" then [] else loop 0 0

let find_one matrix =
  let rec find i j =
    if matrix.(i).(j) = 1 then (i, j)
    else if j < 4 then find i (j + 1)
    else find (i + 1) 0
  in
  find 0 0

let () =
  for i = 0 to 4 do
    let line = read_line () |> split_on_char |> Array.of_list in
    matrix.(i) <- line
  done;
  let i, j = find_one matrix in
  let answer = abs (2 - i) + abs (2 - j) in
  print_int answer
