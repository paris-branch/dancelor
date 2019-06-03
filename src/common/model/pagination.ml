type t =
  { start : int ;
    end_ : int }
[@@deriving yojson]

let _key = "pagination"

let start p = p.start
let end_ p = p.end_

let make () =
  { start = 0 ; end_ = max_int }
