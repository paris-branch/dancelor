type key = string
type query = (key * string list) list

val query_strings_opt : query -> key -> string list option Lwt.t

val query_strings : ?or_:string list -> query -> key -> string list Lwt.t
(** Looks for a string associated to the given key in the query. If the key is
    absent, returns [or_] if provided or fails with [EntityDoesNotExist]. *)

val query_string_opt : query -> key -> string option Lwt.t
(** Looks for a string associated to the given key in the query. If the key is
    absent, returns [None]. If the key is present but has several bindings,
    fails with [BadQuery]. *)

val query_string : ?or_:string -> query -> key -> string Lwt.t
(** Looks for a string associated to the given key in the query. If the key is
    absent, returns [or_] if provided or fails with [EntityDoesNotExist]. If the
    key is present by has several bindings, fails with [BadQuery]. *)

val query_int_opt : query -> key -> int option Lwt.t
(** Looks for an int associated to the given key in the query. If the key is
    absent, returns [None]. If the key is present but has several bindings, or
    only one binding that is not an int, fails with [BadQuery]. *)

val query_int : ?or_:int -> query -> key -> int Lwt.t

val query_float_opt : query -> key -> float option Lwt.t
val query_float : ?or_:float -> query -> key -> float Lwt.t
