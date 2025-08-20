open Nes
open Html

let prepare (type comp_value)(type comp_raw_value)(type params)(type params_previewed)(type params_state)(type raw_params)
  (component : (comp_value, comp_raw_value) Component.s)
  (editor : (params, params_previewed, params_state, raw_params) Editor.s)
  : (comp_value * params, comp_raw_value * raw_params) Component.s
= (module struct
  module C = (val component)

  type value = C.value * params

  let raw_params_of_yojson = Editor.raw_value_of_yojson editor
  let raw_params_to_yojson = Editor.raw_value_to_yojson editor

  type raw_value = C.raw_value * raw_params [@@deriving yojson]

  let empty_value = (C.empty_value, Editor.empty_value editor)

  let raw_value_from_initial_text (text : string) =
    (C.raw_value_from_initial_text text, Editor.empty_value editor)

  let serialise (value, params) =
    let%lwt value = C.serialise value in
    let%lwt params = Editor.serialise editor params in
    lwt (value, params)

  type t = {
    comp: (comp_value, comp_raw_value) Component.t;
    editor: (params, params_previewed, params_state, raw_params) Editor.t;
  }

  let initialise (initial_value, initial_params) =
    let%lwt comp = Component.initialise component initial_value in
    let%lwt editor = Editor.initialise editor @@ Editor.QuickEdit initial_params in
    lwt {comp; editor}

  let signal p =
    RS.bind (Component.signal p.comp) @@ fun value ->
    RS.bind (Editor.signal p.editor) @@ fun params ->
    S.const (Ok (value, params))

  let raw_signal p =
    S.bind (Component.raw_signal p.comp) @@ fun raw_value ->
    S.bind (Editor.raw_signal p.editor) @@ fun raw_params ->
    S.const (raw_value, raw_params)

  let set p (raw_value, raw_params) =
    Component.set p.comp raw_value;
    Editor.set_raw_value p.editor raw_params

  let clear p =
    Component.clear p.comp;
    Editor.clear p.editor

  let label = C.label
  let inner_html p = Component.inner_html p.comp
  let focus p = Component.focus p.comp
  let trigger p = Component.trigger p.comp

  let actions p =
    S.l2
      (@)
      (
        S.const [
          Utils.Button.make
            ~icon: "toggles"
            ~classes: ["btn-outline-secondary"]
            ~tooltip: "Edit parameters"
            ~onclick: (fun () ->
              ignore
              <$> Page.open_dialog' @@ fun return ->
                Editor.page
                  p.editor
                  ~after_save: (fun _ ->
                    Utils.Toast.open_ ~title: "Set parameters" [txt "Your parameters have been set."];
                    return ()
                  )
            )
            ()
        ]
      )
      (Component.actions p.comp)
end)
