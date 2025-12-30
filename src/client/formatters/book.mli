open Common
open Html

val title' :
  ?link: bool ->
  ?context: Endpoints.Page.context ->
  Model.Book.entry ->
  [> Html_types.span] elt

val editors' :
  Model.Book.entry ->
  [> Html_types.span] elt

val date_and_editors :
  Model.Book.t ->
  [> Html_types.span] elt

val date_and_editors' :
  Model.Book.entry ->
  [> Html_types.span] elt
