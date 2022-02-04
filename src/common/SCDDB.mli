(** {1 SCDDB}

    This module contains helpers to create and parse SCDDB URLs. *)

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

val entry_url : entry -> string
(** Given an entry, produce a URL to this entry. *)
(* FIXME: should use the URI type. *)

val dance_url : entry_id -> string
val formation_url : entry_id -> string
val person_url : entry_id -> string
val publication_url : entry_id -> string
val album_url : entry_id -> string
val recording_url : entry_id -> string
val tune_url : entry_id -> string
val list_url : entry_id -> string
(** Given an entry id, produce a URL to the corresponding entry. *)
