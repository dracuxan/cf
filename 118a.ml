let word = read_line ()
let vowels = [ "A"; "O"; "Y"; "E"; "U"; "I" ]

let rec exists c l =
  match l with [] -> false | h :: t -> if h = c then true else exists c t

let rec remove_vowels vowels word n new_word =
  if n >= String.length word then new_word
  else
    (* NOTE: use `lowercase` and `capitalize` when submitting to cf *)
    let current = word.[n] |> String.make 1 |> String.lowercase_ascii in
    if exists (String.capitalize_ascii current) vowels then
      remove_vowels vowels word (n + 1) new_word
    else remove_vowels vowels word (n + 1) (new_word ^ "." ^ current)

let () = print_string (remove_vowels vowels word 0 "")
