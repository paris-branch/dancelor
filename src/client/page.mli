(** {1 Page}

    NOTE: This notion of page is different from the one in the PageRouter.
    FIXME: Rename one of the two! *)

open Html

type t
(** The abstract type of a page in Dancelor. *)

val make :
  ?parent_title: string ->
  title: string S.t ->
  ?before_title: Html_types.div_content_fun elt list ->
  ?footer: Html_types.div_content_fun elt list ->
  Html_types.div_content_fun elt list ->
  t
(** Page maker. The [?parent_title] argument is used to build a title of the
    form ["page | parent page"]. It is empty by default. *)

val full_title : t -> string S.t
(** Full title, that is the title with the parent component. *)

val content : t -> Html_types.div_content_fun elt list
(** Content of the back. *)
