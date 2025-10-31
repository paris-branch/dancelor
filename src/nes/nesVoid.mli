(** {1 Void} *)

type t
(** Void type: entirely uninhabited. *)

val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
