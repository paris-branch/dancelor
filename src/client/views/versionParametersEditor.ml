open Nes
open Components
open Html

let editor =
  let open Editor in
  Input.prepare
    ~type_: Text
    ~label: "Display name (optional)"
    ~placeholder: "eg. The Cairdin o' It"
    ~serialise: (Option.value ~default: "")
    ~validate: (S.const % ok % Option.of_string_nonempty)
    () ^::
  Input.prepare
    ~type_: Text
    ~label: "Display composer (optional)"
    ~placeholder: "eg. Niel Gow"
    ~serialise: (Option.value ~default: "")
    ~validate: (S.const % ok % Option.of_string_nonempty)
    () ^::
  nil

let preview (display_name, (display_composer, ())) =
  lwt_some @@ Model.VersionParameters.make ?display_name ?display_composer ()

let submit _mode params = lwt params

let break_down params =
  let display_name = Model.VersionParameters.display_name params in
  let display_composer = Model.VersionParameters.display_composer params in
  let value = (display_name, (display_composer, ())) in
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
