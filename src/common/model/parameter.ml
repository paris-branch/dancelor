
module Self = struct
  type 'a t = Undefined | Defined of 'a
  [@@deriving yojson]

  let _key = "parameter"
end
include Self

let defined p =  Defined p

let get ?default = function
  | Defined parameter -> parameter
  | Undefined ->
    match default with
    | None -> failwith "Parameter.get"
    | Some default -> default

let compose ?(tie=(fun _ _2 -> _2)) p1 p2 =
  match p1, p2 with
  | Undefined, Undefined -> Undefined
  | Defined p1, Undefined -> Defined p1
  | Undefined, Defined p2 -> Defined p2
  | Defined p1, Defined p2 -> Defined (tie p1 p2)
