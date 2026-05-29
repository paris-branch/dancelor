type t = Sqlgg_postgresql.pg_conn

val with_ : ?transaction: bool -> (t -> 'a Lwt.t) -> 'a Lwt.t
(** Open a connection and passes it to the function, ensuring that the
    connection is closed when the function returns, be it normally or
    exceptionally. The [?transaction] argument, [true] by default, also wraps
    the execution of the function in a PostgreSQL transaction. *)

val with_transaction : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
(** Wrap the execution of the function is a PostgreSQL transaction. The
    transaction is implicitly committed when the function returns, unless it
    raises an exception, in which case it is rolled back. *)

val bypass_exec : t -> string -> unit
(** Escape hatch to execute an arbitrary string against a connection. This is
    only ever meant for exceptional use cases. *)
