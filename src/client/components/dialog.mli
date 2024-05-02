open Dancelor_client_html

type error = Closed
(** The type of errors that can terminate a dialog. *)

val open_ :
  (('result -> unit) -> [< Html_types.div_content_fun] elt list) ->
  ('result, error) Result.t Lwt.t
(** [open_ f] opens a dialog. [f] is used to create the content of the dialog;
    it receives a [return] function that destroys the dialog and make it return.
    [open_ f] returns a promise of either a result or an error. *)
