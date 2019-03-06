open Dancelor_common

(** {1 Tune} *)

type t

val slug : t -> t Slug.t
val name : t -> string
val kind : t -> Kind.base
val author : t -> Credit.t Slug.t option

val to_json : t -> Json.t
val to_jsonm : t -> Json.value

val of_json : Json.t -> t
val of_jsonm : Json.value -> t

val serialize : t -> Json.t
val unserialize : Json.t -> t
