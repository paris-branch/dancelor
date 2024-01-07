(** Start is inclusive; end is non-inclusive. *)
type t =
  { start : int ;
    end_ : int }
[@@deriving yojson]

let _key = "pagination"

let start p = p.start
let end_ p = p.end_

let make () =
  { start = 0 ; end_ = max_int }

let apply p all =
  let rec apply start end_ = function
    | [] -> []
    | _ :: q when start > 0 -> apply (start - 1) (end_ - 1) q
    | h :: q when end_ > 0 -> h :: apply 0 (end_ - 1) q
    | _ -> []
  in
  apply (start p) (end_ p) all
