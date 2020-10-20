
module Self = struct
  type 'a t = Inherit | That of 'a
  [@@deriving yojson]

  let _key = "parameter"
end
include Self

let apply ~parent = function
  | Inherit -> parent
  | That parameter -> That parameter

let is_that = function
  | Inherit -> false
  | That _ -> true

let check_that parameter =
  if is_that parameter then
    ()
  else
    failwith "check_that"
