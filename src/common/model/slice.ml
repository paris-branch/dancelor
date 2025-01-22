type t = {
  start: int;
  end_incl: int
}
[@@deriving yojson]

let start s = s.start
let length s = s.end_incl - s.start + 1
let end_incl s = s.end_incl
let end_excl s = s.end_incl + 1

let make
    ?(start = 0)
    ?length
    ?end_incl
    ?end_excl
    ()
  =
  let end_incl =
    match length, end_incl, end_excl with
    | None, None, None -> max_int
    | Some length, None, None -> start + length - 1
    | None, Some end_incl, None -> end_incl
    | None, None, Some end_excl -> end_excl - 1
    | _ -> invalid_arg "Slice.make"
  in
  if start < 0 || end_incl < start - 1 then
    invalid_arg "Slice.make";
  {start; end_incl}

let nothing = make ~end_excl: 0 ()
let everything = make ~end_incl: max_int ()

let list ?(strict = true) s xs =
  let rec list i = function
    | [] when strict && s.end_incl <> i - 1 -> invalid_arg "Slice.list"
    | _ :: xs when s.start > i -> list (i + 1) xs
    | x :: xs when s.end_incl >= i -> x :: list (i + 1) xs
    | _ -> []
  in
  list 0 xs
