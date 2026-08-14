let word = read_line ()

let rec check_hello word i j =
  let target = "hello" in
  if j >= 5 then print_string "YES"
  else if i >= String.length word then print_string "NO"
  else if word.[i] = target.[j] then check_hello word (i + 1) (j + 1)
  else check_hello word (i + 1) j

let () = check_hello word 0 0
