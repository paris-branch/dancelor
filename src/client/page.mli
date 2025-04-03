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
  ?buttons: Html_types.div_content_fun elt list ->
  Html_types.div_content_fun elt list ->
  t
(** Page maker. The [?parent_title] argument is used to build a title of the
    form ["page | parent page"]. It is empty by default. *)

val full_title : t -> string S.t
(** Full title, that is the title with the parent component. *)

val render : t -> Html_types.div elt
(** Render the page. *)

val open_dialog :
  (('result option -> unit) -> t) ->
  'result option Lwt.t
(** [open_dialog f] opens a dialog. [f] is used to create the content of the
    dialog; it receives a [return] function that destroys the dialog and make it
    return. [open_dialog f] returns a promise of either a result or [None] if
    the dialog was closed. *)

val open_dialog' :
  (('result -> unit) -> t) ->
  'result option Lwt.t
(** Variant of {!open_dialog} where there is always a result. *)
