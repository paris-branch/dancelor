(** {1 Void} *)

type t [@@deriving eq, show, yojson]
(** Void type: entirely uninhabited. *)

val f : t -> 'anything
(** Since there are no void values, they can be converted to anything. *)
