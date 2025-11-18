(** {1 SCDDB}

    This module contains helpers to create and parse SCDDB URIs. *)

(** {2 Definitions} *)

type entry_type =
  | Dance
  | Formation
  | Person
  | Publication
  | Album
  | Recording
  | Tune
  | List
[@@deriving yojson]
(** The type of an entry in the SCDDB. *)

type entry_id = int
[@@deriving yojson]
(** The type of an id in the SCDDB. Such ids only make sense for a given entry
    type. *)

(** {2 Entry to URI} *)

val entry_uri : entry_type -> entry_id -> Uri.t
val dance_uri : entry_id -> Uri.t
val person_uri : entry_id -> Uri.t
val publication_uri : entry_id -> Uri.t
val tune_uri : entry_id -> Uri.t

(** {2 Entry from URI} *)

val entry_from_string : entry_type -> string -> (entry_id, string) result
(** Given a string, attempts to parse it into an entry of the expected type. The
    string can either be a URI of the right type, or simply an id. *)
