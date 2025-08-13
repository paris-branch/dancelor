open Nes
open Html

module type S = sig
  val label : string

  type value
  type raw_value

  val empty_value : raw_value
  val raw_value_from_initial_text : string -> raw_value
  val raw_value_to_yojson : raw_value -> Yojson.Safe.t
  val raw_value_of_yojson : Yojson.Safe.t -> (raw_value, string) result
  val serialise : value -> raw_value Lwt.t

  type t

  val make : raw_value -> t

  val signal : t -> (value, string) result S.t
  val raw_signal : t -> raw_value S.t
  val focus : t -> unit
  val set : t -> raw_value -> unit
  val trigger : t -> unit
  val clear : t -> unit
  val inner_html : t -> Html_types.div_content_fun elt
  val actions : t -> Html_types.div_content_fun elt list S.t
end

type ('value, 'raw_value) s = (module S with type value = 'value and type raw_value = 'raw_value)

type ('value, 'raw_value) t =
  Component : (module S with type t = 'a and type value = 'value and type raw_value = 'raw_value) * 'a -> ('value, 'raw_value) t

let initialise (type value)(type raw_value)
    (module C : S with type value = value and type raw_value = raw_value)
    (initial_value : raw_value)
    : (value, raw_value) t
  =
  Component ((module C), C.make initial_value)

let make (type value)(type raw_value)
    (module C : S with type value = value and type raw_value = raw_value)
    (initial_value : raw_value)
    : (value, raw_value) t
  =
  initialise (module C) initial_value

let focus : type value raw_value. (value, raw_value) t -> unit = function Component ((module C), c) -> C.focus c
let trigger : type value raw_value. (value, raw_value) t -> unit = function Component ((module C), c) -> C.trigger c
let clear : type value raw_value. (value, raw_value) t -> unit = function Component ((module C), c) -> C.clear c
let signal : type value raw_value. (value, raw_value) t -> (value, string) result S.t = function Component ((module C), c) -> C.signal c
let raw_signal : type value raw_value. (value, raw_value) t -> raw_value S.t = function Component ((module C), c) -> C.raw_signal c
let set : type value raw_value. (value, raw_value) t -> raw_value -> unit = function Component ((module C), c) -> C.set c
let inner_html : type value raw_value. (value, raw_value) t -> Html_types.div_content_fun elt = function Component ((module C), c) -> C.inner_html c
let actions : type value raw_value. (value, raw_value) t -> Html_types.div_content_fun elt list S.t = function Component ((module C), c) -> C.actions c

let case_errored ~no ~yes signal =
  flip S.map signal @@ function
    | Error msg -> yes msg
    | _ -> no

let html : type value raw_value. (value, raw_value) t -> [> Html_types.div] elt = function
  | Component ((module C), c) ->
    div
      ~a: [a_class ["mb-2"]]
      [
        div ~a: [a_class ["row"; "align-items-center"]] [
          label ~a: [a_class ["col"]] [txt C.label];
          R.div ~a: [a_class ["col-auto"]] (
            flip S.map (C.actions c) @@ function
              | [] -> [Utils.Button.make ~classes: ["invisible"] ~icon: "plus-circle" ()] (* for spacing *)
              | actions -> actions
          );
        ];
        C.inner_html c;
        R.div
          ~a: [R.a_class (case_errored ~no: ["d-block"; "valid-feedback"] ~yes: (const ["d-block"; "invalid-feedback"]) (C.signal c))]
          (case_errored ~no: [txt " "] ~yes: (List.singleton % txt) (C.signal c));
      ]

let html_fake ~label: label_ content =
  div
    ~a: [a_class ["mb-2"]]
    [
      label ~a: [a_class ["form-label"]] [txt label_];
      content;
      div ~a: [a_class ["d-block"; "valid-feedback"]] [txt " "];
    ]
