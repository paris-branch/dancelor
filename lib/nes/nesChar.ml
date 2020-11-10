include Char

let is_upper c = ('A' <= c && c <= 'Z')
let is_lower c = ('a' <= c && c <= 'z')
let is_letter c = is_upper c || is_lower c

let is_digit c = ('0' <= c && c <= '9')

let is_space c = c = ' '

let is_symbol c = ('!' <= c && c <= '/') || (':' <= c && c <= '@')
                  || ('[' <= c && c <= '`') || ('{' <= c && c <= '~')

type kind = Letter | Digit | Space | Symbol | Other

let kind c =
  if is_letter c then Letter
  else if is_digit c then Digit
  else if is_space c then Space
  else if is_symbol c then Symbol
  else Other

let is_other c = kind c = Other

let%test _ = List.init 256 Char.chr |> List.for_all (fun c -> not (is_letter c && is_digit  c))
let%test _ = List.init 256 Char.chr |> List.for_all (fun c -> not (is_letter c && is_space  c))
let%test _ = List.init 256 Char.chr |> List.for_all (fun c -> not (is_letter c && is_symbol c))
let%test _ = List.init 256 Char.chr |> List.for_all (fun c -> not (is_letter c && is_other  c))
let%test _ = List.init 256 Char.chr |> List.for_all (fun c -> not (is_digit  c && is_space  c))
let%test _ = List.init 256 Char.chr |> List.for_all (fun c -> not (is_digit  c && is_symbol c))
let%test _ = List.init 256 Char.chr |> List.for_all (fun c -> not (is_digit  c && is_other  c))
let%test _ = List.init 256 Char.chr |> List.for_all (fun c -> not (is_space  c && is_symbol c))
let%test _ = List.init 256 Char.chr |> List.for_all (fun c -> not (is_space  c && is_other  c))
let%test _ = List.init 256 Char.chr |> List.for_all (fun c -> not (is_symbol c && is_other  c))

(* Define a sensible order of characters, where they occur in order: space,
   letters, digits, symbols, others. We first define a function from char code
   to integers that reorders them. We pre-compute it for all characters. *)

let sensible_order = function
  | c when   0 <= c && c <= 31  (* other *)        -> c + 69
  | 32                          (* space *)        -> 0
  | c when  33 <= c && c <=  47 (* symbol *)       -> c + 4
  | c when  48 <= c && c <=  57 (* digits *)       -> c - 21
  | c when  58 <= c && c <=  64 (* symbol *)       -> c - 6
  | c when  65 <= c && c <=  90 (* upper letter *) -> c - 64
  | c when  91 <= c && c <=  96 (* symbol *)       -> c - 32
  | c when  97 <= c && c <= 122 (* lower letter *) -> c - 96
  | c when 123 <= c && c <= 126 (* symbol *)       -> c - 58
  | c when 127 <= c && c <= 255 (* other *)        -> c - 26
  | _ -> failwith "NesChar.sensible_order"

let%test _ = List.init 256 sensible_order |> List.for_all (fun c -> 0 <= c && c <= (255 - 26))

let sensible_order =
  let sensible_order = Array.init 256 sensible_order in
  fun c -> sensible_order.(Char.code c)

let sensible_compare c1 c2 =
  Stdlib.compare (sensible_order c1) (sensible_order c2)

let%test _ =
  List.init 256 Char.chr |> List.for_all @@ fun c1 ->
  List.init 256 Char.chr |> List.for_all @@ fun c2 ->
  match kind c1, kind c2 with
  | Space, Letter | Space, Digit | Space, Symbol | Space, Other
  | Letter, Digit | Letter, Symbol | Letter, Other
  | Digit, Symbol | Digit, Other
  | Symbol, Other ->
    sensible_compare c1 c2 < 0
  | _ ->
    true
