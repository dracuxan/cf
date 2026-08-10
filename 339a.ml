let split_string word =
  let rec loop i acc = if i < 0 then acc else loop (i - 1) (word.[i] :: acc) in
  loop (String.length word - 1) []

let equation = read_line () |> split_string

let rec find_nums equation =
  match equation with
  | [] -> []
  | h :: t -> if h = '+' then find_nums t else int_of_char h :: find_nums t

let rec insert x sorted_list =
  match sorted_list with
  | [] -> [ x ]
  | h :: t -> if x <= h then x :: h :: t else h :: insert x t

let rec arrange_nums l =
  match l with [] -> [] | h :: t -> insert h (arrange_nums t)

let rec printer equation =
  match equation with
  | [] -> print_char '\n'
  | h :: [] -> print_char (char_of_int h)
  | h :: t ->
      print_char (char_of_int h);
      print_char '+';
      printer t

let () = equation |> find_nums |> arrange_nums |> printer
