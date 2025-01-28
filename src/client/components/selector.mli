open Nes
open Dancelor_common
open Model
open Html

type ('arity, 'model) t
(** The type of a selector of arity ['arity] and carrying ['model]s. The arity
    describes whether the selector can be used to select exactly one element, or
    a list of elements. *)

type one
type many
type 'any arity
val one : one arity
val many : many arity
(** Type trickeries to keep track of the arity of a selector. You only need to
    care about passing values {!one} or {!many} to {!make}. *)

val make :
  arity: 'arity arity ->
  ?has_interacted: bool S.t ->
  search: (Slice.t -> string -> (int * 'model Database.Entry.t list, string) Result.t Lwt.t) ->
  serialise: ('model Database.Entry.t -> 'model Slug.t) ->
  unserialise: ('model Slug.t -> 'model Database.Entry.t Lwt.t) ->
  (string * 'model Slug.t list) ->
  ('arity, 'model) t

val raw_signal : ('arity, 'model) t -> (string * 'model Slug.t list) S.t
(** The raw signal of the current input of the bar and the elements contained in
    the selector. Independently from the arity, it contains a list. *)

val signal_one : (one, 'model) t -> ('model Database.Entry.t, string) Result.t S.t
(** The actual signal for an arity-one selector. This contains exactly one
    ['model], or an error. *)

val signal_many : (many, 'model) t -> ('model Database.Entry.t list, 'bottom) Result.t S.t
(** The actual signal for an arity-many selector. This always contains a
    {!Result.Ok}. *)

val clear : ('arity, 'model) t -> unit

val render :
  make_result:
    (?classes: string list ->
     ?action: Utils.ResultRow.action ->
     ?prefix: Utils.ResultRow.cell list ->
     ?suffix: Utils.ResultRow.cell list ->
     'model Database.Entry.t ->
     Utils.ResultRow.t) ->
  ?make_more_results:
    ('model Database.Entry.t ->
     Utils.ResultRow.t list) ->
  field_name: (string * string) ->
  model_name: string ->
  create_dialog_content:
    (?on_save: ('model Database.Entry.t -> unit) ->
     string ->
     Html_types.div Html.elt) ->
  ('arity, 'model) t ->
  [> Html_types.div] Html.elt
(** The optional argument [?make_more_results] adds rows to the table after the
    result. This only happens in the result, not in the quick search. *)
