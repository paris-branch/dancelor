open Dancelor_client_model

type cached_tune = {
  slug : string;
  tune : Tune.t;
  group : TuneGroup.t
}

type t

val create : unit -> t

val name : t -> string

val set_name : t -> string -> unit

val kind : t -> string

val set_kind : t -> string -> unit

val count : t -> int

val insert : t -> string -> int -> unit Lwt.t

val add : t -> string -> unit Lwt.t

val get : t -> int -> cached_tune option

val remove : t -> int -> unit

val move_up : t -> int -> unit

val move_down : t -> int -> unit

val iter : t -> (int -> cached_tune -> unit) -> unit

val clear : t -> unit

val save : t -> unit

val load : t -> unit Lwt.t

val erase_storage : t -> unit

val submit : t -> Set.t Lwt.t
