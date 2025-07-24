type t = Yojson.Safe.t

(** {3 Field addition} *)

val add_field : string -> t -> t -> t
(** [add_field key value json] adds the binding [(key, value)] into [json]. *)

val add_fields : (string * t) list -> t -> t
(** [add_fields bindings json] adds all the [bindings] into [json]. *)

(** {3 Field extraction} *)

val extract_field_opt : string -> t -> (t * t) option
(** Returns the value in the field and the JSON value without that field.

    Returns [None] if the field does not exist.

    @raise Invalid_argument if the JSON value is not an [`Assoc]. *)

val extract_field : string -> t -> t * t
(** Returns the value in the field and the JSON value without that field.

    @raise Failure if the field does not exist.
    @raise Invalid_argument if the JSON value is not an [`Assoc]. *)

val extract_fields : string list -> t -> t * t

(** {3 Field removal} *)

val remove_field : string -> t -> t
(** Returns the JSON value without the given field.

    @raise Failure if the field does not exist.
    @raise Invalid_argument if the JSON value is not an [`Assoc]. *)

val keep_fields : string list -> t -> t
(** Returns the JSON value with only the given fields.

    @raise Failure if the field does not exist.
    @raise Invalid_argument if the JSON value is not an [`Assoc]. *)

(** {3 Other} *)

val map_field : string -> (t -> t) -> t -> t

val from_string : string -> t
val to_string : t -> string

val find_opt : string list -> t -> t option
val find : string list -> t -> t

val get : k: (t -> 'a option) -> string list -> t -> 'a
val get_or : k: (t -> 'a option) -> default: 'a -> string list -> t -> 'a
val get_opt : k: (t -> 'a option) -> string list -> t -> 'a option

val string : t -> string option
val int : t -> int option
val strings : t -> string list option
val list : (t -> 'a) -> t -> 'a list option

val merge_assoc : t -> t -> t
(** Merge two association JSON values.

    @raise Invalid_argument if the given JSON values are not [`Assoc]. *)
