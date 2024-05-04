open Nes
open Js_of_ocaml
open React

module type Storable = sig
  type t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
  val empty : t
end

let store (type t) (key : string) (module V : Storable with type t = t) (value : t) =
  Js.Optdef.iter Dom_html.window##.localStorage @@ fun local_storage ->
  local_storage##setItem
    (Js.string key)
    (Js.string @@ Yojson.Safe.to_string @@ V.to_yojson value)

let retrieve (type t) (key : string) (module V : Storable with type t = t) : t =
  Option.value ~default: V.empty @@
  Js.Optdef.case Dom_html.window##.localStorage (fun () -> None) @@ fun local_storage ->
  Js.Opt.case (local_storage##getItem (Js.string key)) (fun () -> None) @@ fun value ->
  Result.to_option @@ V.of_yojson @@ Yojson.Safe.from_string @@ Js.to_string value

let update (type t) (key : string) (module V : Storable with type t = t) (f : t -> t) =
  store key (module V) @@ f @@ retrieve key (module V)

(** [with_local_storage (module V) signal make] aims at making a “state”
    transparently stored in the local storage using its storable representations
    [V.t]. It takes a storable module [V] and checks whether there is something
    currently stored. If it is the case, it will serve as argument to [make];
    otherwise, {!Storable.empty} will be used. [make] is then called with this
    argument to create the value of type “state”. From that state, [signal] is
    called to get storable representations of the “state”; whenever those
    representation change, the new one will be stored. *)
let with_local_storage
    (type t)
    (key : string)
    (module V : Storable with type t = t)
    (signal : 'a -> t S.t)
    (make : t -> 'a)
  : 'a
  =
  let value = retrieve key (module V) in
  let result = make value in
  let iter = S.map (store key (module V)) @@ signal result in
  Depart.depends ~on:result iter;
  result
