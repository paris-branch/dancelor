open Common

open Html

val title :
  Model.Book.t ->
  [> Html_types.span] elt

val title' :
  ?link: bool ->
  ?context: Endpoints.Page.context ->
  Model.Book.t Entry.t ->
  [> Html_types.span] elt

val editors :
  Model.Book.t ->
  [> Html_types.span] elt

val editors' :
  Model.Book.t Entry.t ->
  [> Html_types.span] elt

val date_and_editors :
  Model.Book.t ->
  [> Html_types.span] elt

val date_and_editors' :
  Model.Book.t Entry.t ->
  [> Html_types.span] elt
