let n = int_of_string (read_line ())

let split_on_char sep s =
  let rec loop i last =
    if i = String.length s then [ String.sub s last (i - last) ]
    else if s.[i] = sep then String.sub s last (i - last) :: loop (i + 1) (i + 1)
    else loop (i + 1) last
  in
  if s = "" then [] else loop 0 0

let problem_solved line =
  let list_of_sols = line |> split_on_char ' ' |> List.map int_of_string in
  if List.fold_left ( + ) 0 list_of_sols >= 2 then 1 else 0

let () =
  let solved = ref 0 in
  for _ = 1 to n do
    let line = read_line () in
    solved := !solved + problem_solved line
  done;
  Printf.printf "%d\n" !solved
