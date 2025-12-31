type t = Yojson.Safe.t

(** {3 Field addition} *)

val add_field : string -> t -> t -> t
(** [add_field key value json] adds the binding [(key, value)] into [json]. *)

(** {3 Field extraction} *)

val extract_field_opt : string -> t -> (t * t) option
(** Returns the value in the field and the JSON value without that field.

    Returns [None] if the field does not exist.

    @raise Invalid_argument if the JSON value is not an [`Assoc]. *)

val extract_field : string -> t -> t * t
(** Returns the value in the field and the JSON value without that field.

    @raise Failure if the field does not exist.
    @raise Invalid_argument if the JSON value is not an [`Assoc]. *)

(** {3 Field removal} *)

val keep_fields : string list -> t -> t
(** Returns the JSON value with only the given fields.

    @raise Failure if the field does not exist.
    @raise Invalid_argument if the JSON value is not an [`Assoc]. *)

(** {3 Other} *)

val from_string : string -> t

val get_opt : k: (t -> 'a option) -> string list -> t -> 'a option

val string : t -> string option
val int : t -> int option

val merge_assoc_l : t list -> t
(** Merge several association JSON values.

    @raise Invalid_argument if the given JSON values are not [`Assoc]. *)
