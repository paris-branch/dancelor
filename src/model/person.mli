open Dancelor_common

(** {1 Person} *)

type t

val to_json : t -> Json.t
val to_jsonm : t -> Json.value

val of_json : Json.t -> t
val of_jsonm : Json.value -> t

val slug : t -> t Slug.t
val name : t -> string

(** {2 Unsafe} *)

val serialize : t -> Json.t
val unserialize : Json.t -> t
