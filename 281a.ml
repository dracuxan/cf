let word = read_line ()

let () =
  (* NOTE: use Char.uppercase for ocaml compiler version 4.02 and older *)
  Printf.printf "%c" (Char.uppercase_ascii word.[0]);
  Printf.printf "%s" (String.sub word 1 (String.length word - 1))
