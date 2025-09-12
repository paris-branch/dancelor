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

let disassemble params =
  let display_name = Model.SetParameters.display_name params in
  lwt (display_name, ())

let e =
  Editor.prepare_nosubmit
    ~key: "set parameters"
    ~icon: "fixme"
    editor
    ~assemble
    ~disassemble
    ~check_result: Model.SetParameters.equal
    ~format: (fun _ -> assert false)
    ~href: (fun _ -> assert false)

let empty_value () = Editor.result_to_state e Model.SetParameters.none
