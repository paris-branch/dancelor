type 'a t = L of 'a list [@@deriving eq, show]

let to_list (L xs) = xs

let of_list = function [] -> None | xs -> Some (L xs)

let of_list_exn = function
  | [] -> invalid_arg "NesNonEmptyList.of_list_exn: empty list"
  | xs -> L xs

type 'a mylist = 'a list [@@deriving yojson]

let of_yojson a_of_yojson json =
  Result.bind (mylist_of_yojson a_of_yojson json) @@ fun xs ->
  Option.to_result ~none: "empty list" (of_list xs)

let to_yojson a_to_yojson (L xs) =
  mylist_to_yojson a_to_yojson xs

let map f (L xs) = L (List.map f xs)

let hd (L xs) = List.hd xs
let tl (L xs) = List.tl xs

let singleton x = L [x]
let append (L xs) (L ys) = L (xs @ ys)

let fold_left1 f (L xs) = List.fold_left f (List.hd xs) (List.tl xs)
