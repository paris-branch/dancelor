include Format

let with_formatter_to_string_gen (f : formatter -> 'a) : (string * 'a) =
  let buf = Buffer.create 1024 in
  let fmt = formatter_of_buffer buf in
  let v = f fmt in
  pp_print_flush fmt ();
  (Buffer.contents buf, v)

let with_formatter_to_string (f : formatter -> unit) : string =
  fst (with_formatter_to_string_gen f)

let pp_multiline_sensible name fmt s =
  let rec remove_empty_first_lines = function
    | [] -> []
    | "" :: lines -> remove_empty_first_lines lines
    | lines -> lines
  in
  let rec remove_empty_last_lines = function
    | [] -> []
    | "" :: lines ->
      (
        match remove_empty_last_lines lines with
        | [] -> []
        | lines -> "" :: lines
      )
    | line :: lines ->
      line :: remove_empty_last_lines lines
  in
  s
  |> String.split_on_char '\n'
  |> remove_empty_first_lines
  |> remove_empty_last_lines
  |> (function [] -> ["(empty)"] | s -> s)
  |> (
    function
    | [] -> assert false
    | [line] -> Format.fprintf fmt "%s: %s" name line
    | lines ->
      Format.fprintf fmt "%s:" name;
      List.iter (Format.fprintf fmt "@\n%s") lines
  )
