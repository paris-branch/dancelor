open Nes
open Common

open Html

type ('arity, 'obj, 'serialised) t
(** The type of a selector of arity ['arity] and carrying ['obj]s. The arity
    describes whether the selector can be used to select exactly one element, or
    a list of elements. *)

type ('arity, 'model) model = ('arity, 'model Entry.t, 'model Entry.Id.t) t
(** Specialised version of a selector for models. *)

type one
type many
type 'any arity
val one : one arity
val many : many arity
(** Type trickeries to keep track of the arity of a selector. You only need to
    care about passing values {!one} or {!many} to {!make}. *)

val make :
  arity: 'arity arity ->
  search: (Slice.t -> string -> (int * 'obj list, string) Result.t Lwt.t) ->
  serialise: ('obj -> 'serialised) ->
  unserialise: ('serialised -> 'obj Lwt.t) ->
  'serialised list ->
  ('arity, 'obj, 'serialised) t

val raw_signal : ('arity, 'obj, 'serialised) t -> 'serialised list S.t
(** The raw signal of the elements contained in the selector. Independently from
    the arity, it contains a list. *)

val signal_one : (one, 'obj, 'serialised) t -> ('obj, string) Result.t S.t
(** The actual signal for an arity-one selector. This contains exactly one
    ['obj], or an error. *)

val signal_many : (many, 'obj, 'serialised) t -> ('obj list, 'bottom) Result.t S.t
(** The actual signal for an arity-many selector. This always contains a
    {!Result.Ok}. *)

val clear : ('arity, 'obj, 'serialiased) t -> unit

val render :
  make_result:
  (?classes: string list ->
  ?action: Utils.ResultRow.action ->
  ?prefix: Utils.ResultRow.cell list ->
  ?suffix: Utils.ResultRow.cell list ->
  'obj ->
  Utils.ResultRow.t) ->
  ?make_more_results:
  ('obj ->
  Utils.ResultRow.t list) ->
  field_name: string ->
  object_name: string ->
  create_dialog_content:
  (?on_save: ('obj -> unit) ->
  string ->
  Page.t Lwt.t) ->
  ('arity, 'obj, 'serialiased) t ->
  [> Html_types.div] Html.elt
(** The optional argument [?make_more_results] adds rows to the table after the
    result. This only happens in the result, not in the quick search. *)
