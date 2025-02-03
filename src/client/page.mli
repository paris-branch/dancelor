(** {1 Page}

    NOTE: This notion of page is different from the one in the PageRouter.
    FIXME: Rename one of the two! *)

type t
(** The abstract type of a page in Dancelor. *)

val make :
  ?parent_title: string ->
  title: string React.S.t ->
  Html_types.div Html.elt ->
  t
(** Page maker. The [?parent_title] argument is used to build a title of the
    form ["page | parent page"]. It is empty by default. *)

val get_title : t -> string React.S.t

val get_content : t -> Html_types.div Html.elt
