open Nes
open Html

module type S = sig
  type value
  type raw_value
  type t

  val label : string

  val make : raw_value -> t
  val signal : t -> (value, string) result S.t
  val raw_signal : t -> raw_value S.t
  val focus : t -> unit

  val trigger : t -> unit
  (** Trigger the component. For simple components, this is akin to {!focus}.
      For components with a button triggering an action, though, {!focus} will
      only focus the button, while {!trigger} will trigger the action. *)

  val clear : t -> unit
  val inner_html : t -> Html_types.div_content_fun elt
  val html : t -> [> Html_types.div] elt
  val empty_value : raw_value
end

let case_errored ~no ~yes signal =
  flip S.map signal @@ function
    | Error msg -> yes msg
    | _ -> no

let render ?label: label_ ~signal component =
  div
    ~a: [a_class ["mb-2"]]
    [
      label ~a: [a_class ["form-label"]] (Option.to_list (Option.map txt label_));
      component;
      R.div
        ~a: [R.a_class (case_errored ~no: ["d-block"; "valid-feedback"] ~yes: (const ["d-block"; "invalid-feedback"]) signal)]
        (case_errored ~no: [txt " "] ~yes: (List.singleton % txt) signal);
    ]
