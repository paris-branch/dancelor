include Format

let with_formatter_to_string_gen (f : formatter -> 'a) : (string * 'a) =
  let buf = Buffer.create 1024 in
  let fmt = formatter_of_buffer buf in
  let v = f fmt in
  pp_print_flush fmt ();
  (Buffer.contents buf, v)

let with_formatter_to_string (f : formatter -> unit) : string =
  fst (with_formatter_to_string_gen f)
