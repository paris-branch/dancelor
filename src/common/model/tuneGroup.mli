open Nes

(** {1 Tune} *)

type t

val slug : t -> t Slug.t Lwt.t
val name : t -> string Lwt.t
val kind : t -> Kind.base Lwt.t
val author : t -> Credit.t Slug.t option Lwt.t
val remark : t -> string Lwt.t

val to_yojson : t -> Json.t
val of_yojson : Json.t -> (t, string) result
