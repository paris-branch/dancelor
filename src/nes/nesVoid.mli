(** {1 Void} *)

type t [@@deriving yojson]
(** Void type: entirely uninhabited. *)

val f : t -> 'a
(** Since there are no void values, they can be converted to anything. *)
