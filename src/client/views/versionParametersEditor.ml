open Nes
open Components
open Html

let editor =
  let open Editor in
  Input.prepare_option
    ~type_: Text
    ~label: "Display name"
    ~placeholder: "eg. The Cairdin o' It"
    ~serialise: id
    ~validate: (S.const % ok)
    () ^::
  Input.prepare_option
    ~type_: Text
    ~label: "Display composer"
    ~placeholder: "eg. Niel Gow"
    ~serialise: id
    ~validate: (S.const % ok)
    () ^::
  Input.prepare_option
    ~type_: Text
    ~label: "First bar"
    ~placeholder: "eg. 33"
    ~serialise: (NEString.of_string_exn % string_of_int)
    ~validate: (S.const % Option.to_result ~none: "Not a number" % int_of_string_opt % NEString.to_string)
    () ^::
  nil

let preview (display_name, (display_composer, (first_bar, ()))) =
  lwt_some @@ Model.VersionParameters.make ?display_name ?display_composer ?first_bar ()

let submit _mode params = lwt params

let break_down params =
  let display_name = Model.VersionParameters.display_name params in
  let display_composer = Model.VersionParameters.display_composer params in
  let first_bar = Model.VersionParameters.first_bar params in
  let value = (display_name, (display_composer, (first_bar, ()))) in
  (* Check that we aren't silently removing a field.*)
  let%lwt reconstructed = preview value in
  if not (Option.equal Model.VersionParameters.equal (Some params) reconstructed) then
    raise Editor.NonConvertible;
  lwt value

let e =
  Editor.prepare
    ~key: "version parameters"
    ~icon: "fixme"
    editor
    ~preview
    ~submit
    ~break_down
    ~format: (fun _ -> assert false)
    ~href: (fun _ -> assert false)

let empty_value () = Editor.result_to_state e Model.VersionParameters.none
