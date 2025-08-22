open Nes
open Html

let prepare (type comp_value)(type comp_state)(type params)(type params_previewed)(type params_value)(type params_state)
  ((module C): (comp_value, comp_state) Component.s)
  (editor : (params, params_previewed, params_value, params_state) Editor.s)
  : (comp_value * params, comp_state * params_state) Component.s
= (module struct

  type value = C.value * params

  let params_state_of_yojson = Editor.state_of_yojson editor
  let params_state_to_yojson = Editor.state_to_yojson editor

  type state = C.state * params_state [@@deriving yojson]

  let empty = (C.empty, Editor.empty editor)

  let from_initial_text (text : string) =
    (C.from_initial_text text, Editor.empty editor)

  let value_to_state (value, params) =
    let%lwt value = C.value_to_state value in
    let%lwt params = Editor.result_to_state editor params in
    lwt (value, params)

  type t = {
    comp: C.t;
    editor: (params, params_previewed, params_value, params_state) Editor.t;
  }

  let initialise (initial_value, initial_params) =
    let%lwt comp = C.initialise initial_value in
    let%lwt editor = Editor.initialise editor @@ Editor.QuickEdit initial_params in
    lwt {comp; editor}

  let signal p =
    RS.bind (C.signal p.comp) @@ fun value ->
    RS.bind (Editor.signal p.editor) @@ fun params ->
    S.const (Ok (value, params))

  let state p =
    S.bind (C.state p.comp) @@ fun state ->
    S.bind (Editor.state p.editor) @@ fun params_state ->
    S.const (state, params_state)

  let set p (value, params) =
    C.set p.comp value;%lwt
    Editor.set p.editor params

  let clear p =
    C.clear p.comp;
    Editor.clear p.editor

  let label = C.label
  let inner_html p = C.inner_html p.comp
  let focus p = C.focus p.comp
  let trigger p = C.trigger p.comp

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
      (C.actions p.comp)
end)
