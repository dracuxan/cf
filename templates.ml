let split_on_char s =
  let sep = ' ' in
  let rec loop i last =
    if i = String.length s then [ int_of_string (String.sub s last (i - last)) ]
    else if s.[i] = sep then
      int_of_string (String.sub s last (i - last)) :: loop (i + 1) (i + 1)
    else loop (i + 1) last
  in
  if s = "" then [] else loop 0 0

let split_string word =
  let rec loop i acc = if i < 0 then acc else loop (i - 1) (word.[i] :: acc) in
  loop (String.length word - 1) []

let lowercase_ascii ch =
  if ch >= 'A' && ch <= 'Z' then Char.chr (Char.code ch + 32) else ch
