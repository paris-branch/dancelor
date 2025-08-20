open Nes
open Common
open Components
open Html
open Utils

let editor =
  let open Editor in
  Input.prepare
    ~type_: Text
    ~label: "Display name (optional)"
    ~placeholder: "eg. The Cairdin o' It"
    ~serialise: (Option.value ~default: "")
    ~validate: (S.const % ok % Option.of_string_nonempty)
    () ^::
  Selector.prepare_opt
    ~search: (fun slice input ->
      let%rlwt filter = lwt (Filter.Dance.from_string input) in
      ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Dance Search) slice filter
    )
    ~unserialise: Model.Dance.get
    ~make_result: AnyResult.make_dance_result'
    ~label: "For dance (optional)"
    ~model_name: "dance"
    ~create_dialog_content: DanceEditor.create
    () ^::
  nil

let preview (display_name, (for_dance, ())) =
  lwt_some @@ Model.SetParameters.make ?display_name ?for_dance ()

let submit _mode params = lwt params

let break_down params =
  let display_name = Model.SetParameters.display_name params in
  let%lwt for_dance = Model.SetParameters.for_dance params in
  let value = (display_name, (for_dance, ())) in
  (* Check that we aren't silently removing a field.*)
  let%lwt reconstructed = preview value in
  if not (Option.equal Model.SetParameters.equal (Some params) reconstructed) then
    raise Editor.NonConvertible;
  lwt value

let e =
  Editor.prepare
    ~key: "set parameters"
    ~icon: "fixme"
    editor
    ~preview
    ~submit
    ~break_down
    ~format: (fun _ -> assert false)
    ~href: (fun _ -> assert false)

let empty_value () = Editor.serialise e Model.SetParameters.none
