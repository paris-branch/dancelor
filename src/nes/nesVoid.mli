(** {1 Void} *)

type t [@@deriving biniou {alias = false}, yojson]
(** Void type: entirely uninhabited. *)
