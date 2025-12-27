type t = {
  start: int;
  end_incl: int
}
[@@deriving biniou, yojson]

let start s = s.start
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

let seq ?(strict = true) slice xs =
  let rec seq i xs =
    match xs () with
    | Seq.Nil when strict && slice.end_incl <> i - 1 -> invalid_arg "Slice.seq"
    | Seq.Cons (_, xs) when slice.start > i -> seq (i + 1) xs
    | Seq.Cons (x, xs) when slice.end_incl >= i -> Seq.cons x @@ seq (i + 1) xs
    | _ -> Seq.empty
  in
  seq 0 xs

let list ?strict slice l = List.of_seq @@ seq ?strict slice @@ List.to_seq l
