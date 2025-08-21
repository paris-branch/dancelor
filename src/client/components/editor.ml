open Js_of_ocaml
open Nes
open Html

(* Bundles *)

type ('value, 'raw_value) bundle = Bundle of ('value, 'raw_value) Component.s

let cons (type value1)(type raw_value1)(type value2)(type raw_value2)
    (component : (value1, raw_value1) Component.s)
    (Bundle bundle: (value2, raw_value2) bundle)
    : (value1 * value2, raw_value1 * raw_value2) bundle
  =
  Bundle (module struct
    include (val Pair.prepare component bundle)

    let inner_html p =
      div [
        Component.html' (module C1) (c1 p);
        C2.inner_html (c2 p);
      ]
  end)

let (^::) = cons

let nil : (unit, unit) bundle =
  Bundle (module struct
    let label = "Nil bundle"
    type value = unit
    type raw_value = unit [@@deriving yojson]
    let serialise () = lwt_unit
    let empty_value = ()
    let raw_value_from_initial_text _ = ()
    type t = Nil
    let initialise () = lwt Nil
    let signal Nil = S.const (Ok ())
    let raw_signal Nil = S.const ()
    let focus Nil = ()
    let set Nil () = ()
    let trigger Nil = ()
    let clear Nil = ()
    let inner_html Nil = div []
    let actions Nil = S.const []
  end)

(* Helpers *)

exception NonConvertible

let local_storage_key ~key = key ^ "-editor"

let read_local_storage (type value)(type raw_value) ~key (editor : (value, raw_value) Component.s) =
  let module Editor = (val editor) in
  Option.value ~default: Editor.empty_value @@
  Js.Optdef.case Dom_html.window##.localStorage (fun () -> None) @@ fun local_storage ->
  Js.Opt.case (local_storage##getItem (Js.string (local_storage_key ~key))) (fun () -> None) @@ fun value ->
  Result.to_option @@ Editor.raw_value_of_yojson @@ Yojson.Safe.from_string @@ Js.to_string value

let write_local_storage (type value)(type raw_value) ~key (editor : (value, raw_value) Component.s) value =
  let module Editor = (val editor) in
  Js.Optdef.iter Dom_html.window##.localStorage @@ fun local_storage ->
  local_storage##setItem
    (Js.string (local_storage_key ~key))
    (Js.string @@ Yojson.Safe.to_string @@ Editor.raw_value_to_yojson value)

let update_local_storage ~key (Bundle editor) f =
  let x = read_local_storage ~key editor in
  let new_value = f x in
  write_local_storage ~key editor new_value

(* Mode *)

type ('result, 'raw_value) mode =
  | QuickEdit of 'raw_value
  | QuickCreate of string * ('result -> unit)
  | CreateWithLocalStorage
  | Edit of 'result
[@@deriving variants]

(* Prepared editors *)

type ('result, 'previewed_value, 'value, 'raw_value) s = {
  key: string;
  icon: string;
  preview: ('value -> 'previewed_value option Lwt.t);
  submit: (('result, 'raw_value) mode -> 'previewed_value -> 'result Lwt.t);
  break_down: ('result -> 'value Lwt.t);
  format: ('result -> Html_types.div_content_fun Html.elt);
  href: ('result -> string);
  bundle: ('value, 'raw_value) bundle;
}

let empty_value (type value)(type raw_value) {bundle = (Bundle(module C): (value, raw_value) bundle); _} : raw_value = C.empty_value
let raw_value_of_yojson (type value)(type raw_value) {bundle = (Bundle(module C): (value, raw_value) bundle); _} = C.raw_value_of_yojson
let raw_value_to_yojson (type value)(type raw_value) {bundle = (Bundle(module C): (value, raw_value) bundle); _} = C.raw_value_to_yojson
let serialise (type result)(type value)(type raw_value) : (result, 'previewed_value, value, raw_value) s -> result -> raw_value Lwt.t = fun {bundle = Bundle(module C); break_down; _} value -> break_down value >>= C.serialise

let prepare ~key ~icon ~preview ~submit ~break_down ~format ~href bundle =
  {key; icon; preview; submit; break_down; format; href; bundle}

(* Initialised editors *)

type ('result, 'previewed_value, 'value, 'raw_value) t = {
  s: ('result, 'previewed_value, 'value, 'raw_value) s;
  mode: ('result, 'raw_value) mode;
  page: (?after_save: (unit -> unit) -> unit -> Page.t Lwt.t);
  editor: ('value, 'raw_value) Component.t;
}
[@@deriving fields]

let raw_signal e = Component.raw_signal e.editor
let set_raw_value e = Component.set e.editor
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

let initialise (type result)(type value)(type previewed_value)(type raw_value)
    (editor_s : (result, previewed_value, value, raw_value) s)
    (mode : (result, raw_value) mode)
    : (result, previewed_value, value, raw_value) t Lwt.t
  =
  let {key; icon; preview; submit; break_down; format; href; bundle} = editor_s in
  let Bundle bundle = bundle in
  let module Bundle = (val bundle) in

  (* Determine the initial value of the editor. If there is an initial text,
     then we create it from that. If not, we retrieve a maybe-existing value
     from the local storage. *)
  let%lwt initial_value =
    match mode with
    | QuickCreate (initial_text, _) -> lwt @@ Bundle.raw_value_from_initial_text initial_text
    | QuickEdit raw_value -> lwt raw_value
    | CreateWithLocalStorage -> lwt @@ read_local_storage ~key bundle
    | Edit entry -> Bundle.serialise =<< break_down entry
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
      let iter = S.map store @@ Component.raw_signal editor in
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
