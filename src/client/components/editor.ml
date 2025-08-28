open Js_of_ocaml
open Nes
open Html

(* Bundles *)

type ('value, 'state) bundle = Bundle of ('value, 'state) Component.s

let cons (type value1)(type state1)(type value2)(type state2)
    (component : (value1, state1) Component.s)
    (Bundle bundle: (value2, state2) bundle)
    : (value1 * value2, state1 * state2) bundle
  =
  Bundle (module struct
    include (val Cpair.bundle component bundle)

    let inner_html p =
      div [
        Component.html' (module C1) (c1 p);
        C2.inner_html (c2 p);
      ]
  end)

let (^::) = cons
let nil : (unit, unit) bundle = Bundle (Nil.prepare ())

(* Helpers *)

exception NonConvertible

let local_storage_key ~key = key ^ "-editor"

let read_local_storage (type value)(type state) ~key (editor : (value, state) Component.s) =
  let module Editor = (val editor) in
  Option.value ~default: Editor.empty @@
  Js.Optdef.case Dom_html.window##.localStorage (fun () -> None) @@ fun local_storage ->
  Js.Opt.case (local_storage##getItem (Js.string (local_storage_key ~key))) (fun () -> None) @@ fun value ->
  Result.to_option @@ Editor.state_of_yojson @@ Yojson.Safe.from_string @@ Js.to_string value

let write_local_storage (type value)(type state) ~key (editor : (value, state) Component.s) value =
  let module Editor = (val editor) in
  Js.Optdef.iter Dom_html.window##.localStorage @@ fun local_storage ->
  local_storage##setItem
    (Js.string (local_storage_key ~key))
    (Js.string @@ Yojson.Safe.to_string @@ Editor.state_to_yojson value)

let update_local_storage ~key (Bundle editor) f =
  let x = read_local_storage ~key editor in
  let new_value = f x in
  write_local_storage ~key editor new_value

(* Mode *)

type ('result, 'state) mode =
  | QuickEdit of 'state
  | QuickCreate of string * ('result -> unit)
  | CreateWithLocalStorage
  | Edit of 'result
[@@deriving variants]

(* Prepared editors *)

type ('result, 'previewed_value, 'value, 'state) s = {
  key: string;
  icon: string;
  preview: ('value -> 'previewed_value option Lwt.t);
  submit: (('result, 'state) mode -> 'previewed_value -> 'result Lwt.t);
  break_down: ('result -> 'value Lwt.t);
  format: ('result -> Html_types.div_content_fun Html.elt);
  href: ('result -> string);
  bundle: ('value, 'state) bundle;
}

let empty (type value)(type state) {bundle = (Bundle(module C): (value, state) bundle); _} : state = C.empty
let state_of_yojson (type value)(type state) {bundle = (Bundle(module C): (value, state) bundle); _} = C.state_of_yojson
let state_to_yojson (type value)(type state) {bundle = (Bundle(module C): (value, state) bundle); _} = C.state_to_yojson
let result_to_state (type result)(type value)(type state) : (result, 'previewed_value, value, state) s -> result -> state Lwt.t = fun {bundle = Bundle(module C); break_down; _} value -> break_down value >>= C.value_to_state

let prepare ~key ~icon ~preview ~submit ~break_down ~format ~href bundle =
  {key; icon; preview; submit; break_down; format; href; bundle}

(* Initialised editors *)

type ('result, 'previewed_value, 'value, 'state) t = {
  s: ('result, 'previewed_value, 'value, 'state) s;
  mode: ('result, 'state) mode;
  page: (?after_save: (unit -> unit) -> unit -> Page.t Lwt.t);
  editor: ('value, 'state) Component.t;
}
[@@deriving fields]

let state e = Component.state e.editor
let set e r = Component.set e.editor =<< e.s.break_down r
let clear e = Component.clear e.editor

let signal e =
  RS.bind (Component.signal e.editor) @@ fun value ->
  S.from'
    (Error "previsualisation and submission have not finished computing yet")
    (
      match%lwt e.s.preview value with
      | None -> lwt_error "previsualisation failed"
      | Some previewed_value -> ok <$> e.s.submit e.mode previewed_value
    )

let result e =
  match S.value @@ Component.signal e.editor with
  | Error _ -> lwt_none
  | Ok value ->
    match%lwt e.s.preview value with
    | None -> lwt_none
    | Some previewed_value -> some <$> e.s.submit (e.mode) previewed_value

let page ?after_save e = e.page ?after_save ()

let initialise (type result)(type value)(type previewed_value)(type state)
    (editor_s : (result, previewed_value, value, state) s)
    (mode : (result, state) mode)
    : (result, previewed_value, value, state) t Lwt.t
  =
  let {key; icon; preview; submit; break_down; format; href; bundle} = editor_s in
  let Bundle bundle = bundle in
  let module Bundle = (val bundle) in

  (* Determine the initial value of the editor. If there is an initial text,
     then we create it from that. If not, we retrieve a maybe-existing value
     from the local storage. *)
  let%lwt initial_value =
    match mode with
    | QuickCreate (initial_text, _) -> lwt @@ Bundle.from_initial_text initial_text
    | QuickEdit state -> lwt state
    | CreateWithLocalStorage -> lwt @@ read_local_storage ~key bundle
    | Edit entry -> Bundle.value_to_state =<< break_down entry
  in

  (* Now that we have an initial value, we can actually initialise the editor to
     get things running. *)
  let%lwt editor = Component.initialise bundle initial_value in

  (* If there was no initial text, then we connected to the local storage. We
     now enable saving the state of the editor when it changes.. *)
  (
    match mode with
    | QuickCreate _ | QuickEdit _ | Edit _ -> ()
    | CreateWithLocalStorage ->
      let store = write_local_storage ~key bundle in
      let iter = S.map store @@ Component.state editor in
      (* NOTE: Depending on the promise breaks eventually. Depending on the editor
        seems to live as long as the page lives. *)
      Depart.depends ~on: editor iter
  );

  (* What to do when “save” is clicked. *)
  let save ?(after_save = fun () -> Component.clear editor) f =
    let%lwt result =
      match S.value @@ Component.signal editor with
      | Error _ -> lwt_none
      | Ok value ->
        match%lwt preview value with
        | None -> lwt_none
        | Some previewed_value -> some <$> submit mode previewed_value
    in
    (
      Option.iter
        (fun result ->
          after_save ();
          f result;
        )
        result
    );
    lwt_unit
  in
  let save_buttons ?after_save () =
    let button ?label f =
      Utils.Button.save
        ?label
        ~disabled: (S.map Result.is_error (Component.signal editor))
        ~onclick: (fun () -> save ?after_save f)
        ()
    in
    let show_toast result =
      Utils.Toast.open_
        ~title: (String.capitalize_ascii key ^ " created")
        [txt ("The " ^ key ^ " ");
        format result;
        txt " has been created successfully.";
        ]
        ~buttons: [
          Utils.Button.make_a
            ~label: ("Go to " ^ key)
            ~icon
            ~classes: ["btn-primary"]
            ~href: (S.const @@ href result)
            ();
        ]
    in
    let redirect result =
      Dom_html.window##.location##.href := Js.string (href result)
    in
    match mode with
    | QuickCreate (_, on_save) -> [button on_save]
    | QuickEdit _ -> [button (fun _ -> ())]
    | Edit _ -> [button redirect]
    | CreateWithLocalStorage ->
      [
        button ~label: "Save and stay" show_toast;
        button ~label: "Save and see" redirect;
      ]
  in

  (* Make a page holding the editor and the appropriate buttons and actions. *)
  let page ?after_save () =
    Page.make'
      ~title: (
        lwt @@
          match mode with
          | QuickCreate _ | CreateWithLocalStorage -> "Add a " ^ key
          | QuickEdit _ | Edit _ -> "Edit a " ^ key
      )
      ~on_load: (fun () -> Component.focus editor)
      [Component.inner_html editor]
      ~buttons: (
        Utils.Button.clear
          ~onclick: (fun () -> Component.clear editor)
          () :: save_buttons ?after_save ()
      )
  in

  (* Return the fully built editor. *)
  lwt {s = editor_s; mode; page; editor}

(* All-in-one function *)

let make_page ~key ~icon ~preview ~submit ~break_down ~format ~href ~mode bundle =
  page =<< initialise (prepare ~key ~icon ~preview ~submit ~break_down ~format ~href bundle) mode
