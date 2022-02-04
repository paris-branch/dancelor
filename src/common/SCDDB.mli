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
(** The type of an entry in the SCDDB. *)

type entry_id = int
(** The type of an id in the SCDDB. Such ids only make sense for a given entry
   type. *)

type entry
(** Type of an entry in the SCDDB. *)

val entry_type : entry -> entry_type
(** Given an entry, returns its type. *)

val entry_id : entry -> entry_id
(** Given an entry, returns its id. *)

(** {2 Entry to URI} *)

val entry_uri : entry -> string
(** Given an entry, produce a URI to this entry. *)
(* FIXME: should use the Uri.t type. *)

val dance_uri : entry_id -> string
val formation_uri : entry_id -> string
val person_uri : entry_id -> string
val publication_uri : entry_id -> string
val album_uri : entry_id -> string
val recording_uri : entry_id -> string
val tune_uri : entry_id -> string
val list_uri : entry_id -> string
(** Given an entry id, produce a URI to the corresponding entry. *)

(** {2 Entry from URI} *)

val entry_from_uri : string -> (entry, string) result
(** Given an URI, attempts to parse it into an entry. *)
(* FIXME: should use the Uri.t type. *)

val dance_from_uri : string -> (entry_id, string) result
val formation_from_uri : string -> (entry_id, string) result
val person_from_uri : string -> (entry_id, string) result
val publication_from_uri : string -> (entry_id, string) result
val album_from_uri : string -> (entry_id, string) result
val recording_from_uri : string -> (entry_id, string) result
val tune_from_uri : string -> (entry_id, string) result
val list_from_uri : string -> (entry_id, string) result
