open Nes
open Js_of_ocaml
open React

module type Storable = sig
  type t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
  val empty : t
  val _key : string
end

let store (type t) (module V : Storable with type t = t) (value : t) =
  Js.Optdef.iter Dom_html.window##.localStorage @@ fun local_storage ->
  local_storage##setItem
    (Js.string V._key)
    (Js.string @@ Yojson.Safe.to_string @@ V.to_yojson value)

let retrieve (type t) (module V : Storable with type t = t) : t =
  Option.value ~default: V.empty @@
  Js.Optdef.case Dom_html.window##.localStorage (fun () -> None) @@ fun local_storage ->
  Js.Opt.case (local_storage##getItem (Js.string V._key)) (fun () -> None) @@ fun value ->
  Result.to_option @@ V.of_yojson @@ Yojson.Safe.from_string @@ Js.to_string value

let with_local_storage
    (type t)
    (module V : Storable with type t = t)
    (signal : 'a -> t option S.t)
    (make : t -> 'a)
  : 'a
  =
  let value = retrieve (module V) in
  let result = make value in
  let _ = S.map (Option.iter @@ store (module V)) (signal result) in
  result
