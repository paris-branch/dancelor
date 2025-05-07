open Common

open Html

val title_and_subtitle : Model.Book.t -> [> `Br | `PCDATA | `Span] elt list

val title_and_subtitle' :
  Model.Book.t Entry.t ->
  [> `Br | `PCDATA | `Span] elt list

val short_title' :
  ?link: bool ->
  Model.Book.t Entry.t ->
  [> `A of [> Html_types.txt] | `PCDATA] elt list
