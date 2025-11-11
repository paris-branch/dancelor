open Common
open Html

val works' :
  Model.Set.t Entry.t ->
  [> Html_types.span] elt
(** Variant of {!works} taking an {!Entry.t}. *)

val name :
  Model.Set.t ->
  [> Html_types.span] elt

val name' :
  ?link: bool ->
  ?params: Model.SetParameters.t ->
  Model.Set.t Entry.t ->
  [> Html_types.span] elt
(** Variant of {!name} taking an {!Entry.t}. Because this is an entry, we can
    additionnally have a link. *)

val tunes' :
  ?link: bool ->
  Model.Set.t Entry.t ->
  [> Html_types.span] elt
(** Variant of {!tunes} taking an {!Entry.t}. *)

val conceptors' :
  ?short: bool ->
  ?params: Model.SetParameters.t ->
  Model.Set.t Entry.t ->
  [> Html_types.span] elt
(** Variant of {!conceptors} taking an {!Entry.t}. *)
