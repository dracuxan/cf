let n = int_of_string (read_line ())

let abbreviate word =
  let len = String.length word in
  if len > 10 then Printf.sprintf "%c%d%c" word.[0] (len - 2) word.[len - 1]
  else word

let () =
  for _ = 1 to n do
    let word = read_line () in
    Printf.printf "%s\n" (abbreviate word)
  done
