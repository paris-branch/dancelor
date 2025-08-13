(** {1 A fully featured editor}*)

(** {2 Bundle of components}

    An editor features several components, and therefore we provide here a way
    to bundle components together under a list-like structure. *)

type ('value, 'raw_value) bundle

val nil : (unit, unit) bundle

val cons :
  ('value1, 'raw_value1) Component.s ->
  ('value2, 'raw_value2) bundle ->
  ('value1 * 'value2, 'raw_value1 * 'raw_value2) bundle

val (^::):
  ('value1, 'raw_value1) Component.s ->
  ('value2, 'raw_value2) bundle ->
  ('value1 * 'value2, 'raw_value1 * 'raw_value2) bundle
(** [c ^:: cs] is an alias for [cons c cs]. It is right associative. *)

(** {2 Making an editor} *)

type 'result mode =
  | QuickCreate of string * ('result -> unit)
  | CreateWithLocalStorage
  | Edit of 'result
[@@deriving variants]

val make_page :
  key: string ->
  icon: string ->
  preview: ('value -> 'previewed_value option Lwt.t) ->
  submit: ('result mode -> 'previewed_value -> 'result Lwt.t) ->
  break_down: ('result -> 'value Lwt.t) ->
  format: ('result -> Html_types.div_content_fun Html.elt) ->
  href: ('result -> string) ->
  (* FIXME: URI? *)
  mode: 'result mode ->
  ('value, 'raw_value) bundle ->
  Page.t Lwt.t
(** Make a fully-featured editor that takes a whole page.

    [mode] is an argument that decribes what kind of editor we want: is it the
    regular creation editor, connected to local storage, or is it a quick
    creation (eg. called by another editor), in which case it can carry a
    string. It is also passed to the [submit] function.

    [break_down] is a function able to go in the reverse direction from
    [submit]. In the case of models, for instance, that means breaking down the
    model into values for the components, and is used when editing models. This
    function is allowed to raise {!NonConvertible}. *)

(** {2 Advanced use} *)

val update_local_storage :
  key: string ->
  ('value, 'raw_value) bundle ->
  ('raw_value -> 'raw_value) ->
  unit

exception NonConvertible
(** Exception thrown when trying to edit a model that uses features that the
    editor cannot express. *)
