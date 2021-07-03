open Nes
open Dancelor_client_model

type cached_version = {
  slug : Version.t Slug.t;
  version : Version.t;
  tune : Tune.t
}

type t

val create : unit -> t

val name : t -> string

val set_name : t -> string -> unit

val kind : t -> string

val set_kind : t -> string -> unit

val deviser : t -> Credit.t option

val set_deviser : t -> Credit.t Slug.t -> unit Lwt.t

val remove_deviser : t -> unit

val count : t -> int

val insert : t -> Version.t Slug.t -> int -> unit Lwt.t

val add : t -> Version.t Slug.t -> unit Lwt.t

val get : t -> int -> cached_version option

val remove : t -> int -> unit

val move_up : t -> int -> unit

val move_down : t -> int -> unit

val iter : t -> (int -> cached_version -> unit) -> unit

val clear : t -> unit

val save : t -> unit

val load : t -> unit Lwt.t

val erase_storage : t -> unit

val submit : t -> Set.t Lwt.t
