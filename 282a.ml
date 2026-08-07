let n = int_of_string (read_line ())

let split_string word =
  let rec loop i acc = if i < 0 then acc else loop (i - 1) (word.[i] :: acc) in
  loop (String.length word - 1) []

let rec parse list fin =
  match list with
  | [] -> fin
  | '+' :: '+' :: t -> parse t (fin + 1)
  | '-' :: '-' :: t -> parse t (fin - 1)
  | _ :: t -> parse t fin

let () =
  let value = ref 0 in
  for _ = 1 to n do
    let word = read_line () |> split_string in
    value := !value + parse word 0
  done;
  Printf.printf "%d" !value
