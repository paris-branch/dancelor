type t = Sqlgg_postgresql.pg_conn

val with_ : (t -> 'a Lwt.t) -> 'a Lwt.t

val bypass_exec : t -> string -> unit
(** Escape hatch to execute an arbitrary string against a connection. This is
    only ever meant for exceptional use cases. *)
