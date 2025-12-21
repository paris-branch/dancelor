type t = Cohttp.Response.t * Cohttp.Body.t [@@deriving biniou, yojson]

val body_of_lwt : Cohttp_lwt.Body.t -> Cohttp.Body.t Lwt.t
(** Un-lwt-ise a Cohttp body. *)
