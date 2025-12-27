type 'a t = L of 'a list [@@deriving eq, show]

let to_list (L xs) = xs

let of_list = function [] -> None | xs -> Some (L xs)

type 'a mylist = 'a list [@@deriving biniou, yojson]

let of_biniou_exn a_of_biniou biniou =
  Option.get @@ of_list @@ mylist_of_biniou_exn a_of_biniou biniou

let of_biniou a_of_biniou =
  Ppx_deriving_biniou_runtime.of_biniou_of_of_biniou_exn (of_biniou_exn a_of_biniou)

let of_yojson a_of_yojson json =
  Result.bind (mylist_of_yojson a_of_yojson json) @@ fun xs ->
  Option.to_result ~none: "empty list" (of_list xs)

let to_biniou a_to_biniou (L xs) =
  mylist_to_biniou a_to_biniou xs

let to_yojson a_to_yojson (L xs) =
  mylist_to_yojson a_to_yojson xs

let map f (L xs) = L (List.map f xs)
let map_lwt_p f (L xs) = let%lwt ys = Lwt_list.map_p f xs in Lwt.return (L ys)

let hd (L xs) = List.hd xs
let tl (L xs) = List.tl xs

let singleton x = L [x]

let is_singleton (L xs) = match xs with [_] -> true | _ -> false

let mem x (L xs) = List.mem x xs
