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

(** Used by {!with_local_storage} to avoid garbage-collection of the iterators
    that store the state in the local storage. This is inspired by {!S.keep},
    except {!S.keep} does not seem to work in our context and introduces a
    memory leak. *)
let gc_roots = ref []

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
    (module V : Storable with type t = t)
    (signal : 'a -> t option S.t)
    (make : t -> 'a)
  : 'a
  =
  let value = retrieve (module V) in
  let result = make value in
  let iter = S.map (Option.iter @@ store (module V)) @@ signal result in
  gc_roots := iter :: !gc_roots;
  Gc.finalise_last (fun () -> gc_roots := List.filter (not % S.equal iter) !gc_roots) result;
  result
