open Common

open Html

val title_and_subtitle :
  Model.Book.t ->
  [> Html_types.span] elt

val title_and_subtitle' :
  Model.Book.t Entry.t ->
  [> Html_types.span] elt

val short_title' :
  ?link: bool ->
  Model.Book.t Entry.t ->
  [> Html_types.span] elt
