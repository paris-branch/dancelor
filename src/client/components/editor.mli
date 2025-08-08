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

val make_page :
  key: string ->
  icon: string ->
  preview: ('value -> 'previewed_value option Lwt.t) ->
  submit: ('previewed_value -> 'result Lwt.t) ->
  format: ('result -> Html_types.div_content_fun Html.elt) ->
  href: ('result -> string) ->
  (* FIXME: URI? *)
  ?on_save: ('result -> unit) ->
  ?initial_text: string ->
  ('value, 'raw_value) bundle ->
  Page.t Lwt.t
(** Sometimes, editors are created in a context from an initial text, for
    instance a search string, or the name of the object to create. The optional
    argument [?initial_text] allows to carry this value. Beware: the presence of
    this value also disables the connection to local storage, so [Some ""] is
    very different from [None]. *)

val no_preview : 'value -> 'value option Lwt.t
(** A more eloquent alias for {!lwt_some}. *)

(** {2 Advanced use} *)

val update_local_storage :
  key: string ->
  ('value, 'raw_value) bundle ->
  ('raw_value -> 'raw_value) ->
  unit
