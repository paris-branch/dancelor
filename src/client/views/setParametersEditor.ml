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
    nil

let assemble (display_name, ()) =
  Model.SetParameters.make ?display_name ()

let submit _mode params = lwt params

let unsubmit params = lwt params

let disassemble params =
  let display_name = Model.SetParameters.display_name params in
  lwt (display_name, ())

let e =
  Editor.prepare
    ~key: "set parameters"
    ~icon: "fixme"
    editor
    ~assemble
    ~submit
    ~unsubmit
    ~disassemble
    ~format: (fun _ -> assert false)
    ~href: (fun _ -> assert false)

let empty_value () = Editor.result_to_state e Model.SetParameters.none
