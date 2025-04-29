open NesPervasives

let char_is_lowercase c = 'a' <= c && c <= 'z'
let char_is_uppercase c = 'A' <= c && c <= 'Z'
let char_is_number c = '0' <= c && c <= '9'
let char_is_other c = not (char_is_lowercase c || char_is_uppercase c || char_is_number c)

let check s =
  let checks = [
    ((String.length s >= 8), "The password must contain at least 8 characters.");
    ((String.length s <= 64), "The password must contain at most 64 characters.");
    ((String.exists char_is_lowercase s), "The password must contain a lowercase letter.");
    ((String.exists char_is_uppercase s), "The password must contain an uppercase letter.");
    ((String.exists char_is_number s), "The password must contain a number.");
    ((String.exists char_is_other s), "The password must contain a symbol.");
  ]
  in
  match List.find_opt (not % fst) checks with
  | Some (_, reason) -> Error reason
  | None -> Ok ()
