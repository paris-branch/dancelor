(** {1 Void} *)

type t
(** Void type: entirely uninhabited. *)

val f : t -> 'a
(** Since there are no void values, they can be converted to anything. *)

val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
