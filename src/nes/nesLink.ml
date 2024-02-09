(* FIXME: use this for router routes *)

type ('a, 'b) t = 'a -> 'b option

let link fs x =
  let rec link = function
    | [] -> None
    | f :: fs ->
      match f x with
      | Some y -> Some y
      | None -> link fs
  in
  link fs

let link' ~default fs x =
  Option.value ~default (link fs x)

let link_all fs x =
  List.filter_map (fun f -> f x) fs
