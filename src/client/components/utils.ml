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

(** [with_local_storage (module V) signal create_dep make] aims at making a
    “state” transparently stored in the local storage using its storable
    representations [V.t]. It takes a storable module [V] and checks whether
    there is something currently stored. If it is the case, it will serve as
    argument to [make]; otherwise, {!Storable.empty} will be used. [make] is
    then called with this argument to create the value of type “state”. From
    that state, [signal] is called to get storable representations of the
    “state”; whenever those representation change, the new one will be
    stored

    FIXME: [create_dep] is a hack to create a dependency (w.r.t. garbage
    collection) from the “state” to the signal that stores values of type [V.t]
    in the local storage. {!S.keep} does not seem to work, and if it did it
    would probably create a memory leak. It must be possible to achieve the same
    result without having to explicitly create the dependency (at least it is
    possible to explicitly do that in C-OCaml). It is fine for a
    proof-of-concept, but it needs to be fixed later. *)
let with_local_storage
    (type t)
    (module V : Storable with type t = t)
    (signal : 'a -> t option S.t)
    (create_dep : 'a -> 'b -> unit)
    (make : t -> 'a)
  : 'a
  =
  let value = retrieve (module V) in
  let result = make value in
  let iter = S.map (Option.iter @@ store (module V)) @@ signal result in
  let () = create_dep result iter in
  result
