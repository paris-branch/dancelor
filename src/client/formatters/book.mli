open Dancelor_common
open Html

val name' :
  ?link: bool ->
  ?in_search: Endpoints.Page.In_search.t S.t ->
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
