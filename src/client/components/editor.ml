open Js_of_ocaml
open Nes
open Html

exception NonConvertible

type ('value, 'raw_value) bundle = Bundle of ('value, 'raw_value) Component.s

let cons (type value1)(type raw_value1)(type value2)(type raw_value2)
    (component : (value1, raw_value1) Component.s)
    (Bundle bundle: (value2, raw_value2) bundle)
    : (value1 * value2, raw_value1 * raw_value2) bundle
  =
  Bundle (module struct
    module C = (val component)
    module B = (val bundle)

    let label = "Bundle"

    type value = C.value * B.value
    type raw_value = C.raw_value * B.raw_value [@@deriving yojson]

    let empty_value = (C.empty_value, B.empty_value)
    let raw_value_from_initial_text text = (C.raw_value_from_initial_text text, B.empty_value)
    let serialise (v1, v2) = (C.serialise v1, B.serialise v2)

    type t = {
      component: (value1, raw_value1) Component.t;
      bundle: (value2, raw_value2) Component.t;
    }

    let signal l =
      RS.bind (Component.signal l.component) @@ fun component ->
      RS.bind (Component.signal l.bundle) @@ fun bundle ->
      RS.pure (component, bundle)

    let raw_signal l =
      S.bind (Component.raw_signal l.component) @@ fun component ->
      S.bind (Component.raw_signal l.bundle) @@ fun bundle ->
      S.const (component, bundle)

    let focus l = Component.focus l.component
    let trigger l = Component.trigger l.component

    let set l (value1, value2) =
      Component.set l.component value1;
      Component.set l.bundle value2

    let clear l =
      Component.clear l.component;
      Component.clear l.bundle

    let inner_html l =
      div [
        Component.html l.component;
        Component.inner_html l.bundle;
      ]

    let actions _ = S.const []

    let make (initial_value1, initial_value2) = {
      component = Component.make component initial_value1;
      bundle = Component.make bundle initial_value2;
    }
  end)

let (^::) = cons

let nil : (unit, unit) bundle =
  Bundle (module struct
    let label = "Nil bundle"
    type value = unit
    type raw_value = unit [@@deriving yojson]
    let serialise () = ()
    let empty_value = ()
    let raw_value_from_initial_text _ = ()
    type t = Nil
    let make () = Nil
    let signal Nil = S.const (Ok ())
    let raw_signal Nil = S.const ()
    let focus Nil = ()
    let set Nil () = ()
    let trigger Nil = ()
    let clear Nil = ()
    let inner_html Nil = div []
    let actions Nil = S.const []
  end)

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

type 'result mode =
  | QuickCreate of string
  | CreateWithLocalStorage
  | Edit of 'result
[@@deriving variants]

let mode_from_text_or_id get initial_text maybe_id =
  match initial_text, maybe_id with
  | None, None -> lwt @@ CreateWithLocalStorage
  | Some initial_text, None -> lwt @@ QuickCreate initial_text
  | None, Some id -> edit % Option.get <$> get id
  | _ -> invalid_arg "mode_from_text_and_edit"

let make_page (type value)(type raw_value)
    ~key
    ~icon
    ~preview
    ~submit
    ~break_down
    ~format
    ~href
    ?on_save
    ~mode
    (Bundle editor_s: (value, raw_value) bundle)
  =
  let module Editor = (val editor_s) in

  (* Determine the initial value of the editor. If there is an initial text,
     then we create it from that. If not, we retrieve a maybe-existing value
     from the local storage. *)
  let%lwt initial_value =
    match mode with
    | QuickCreate initial_text -> lwt @@ Editor.raw_value_from_initial_text initial_text
    | CreateWithLocalStorage -> lwt @@ read_local_storage ~key editor_s
    | Edit entry -> Editor.serialise <$> break_down entry
  in

  (* Now that we have an initial value, we can actually initialise the editor to
     get things running. *)
  let editor = Component.initialise editor_s initial_value in

  (* What to do when “save” is clicked. *)
  let save () =
    let%lwt result =
      match S.value @@ Component.signal editor with
      | Error _ -> lwt_none
      | Ok value ->
        match%lwt preview value with
        | None -> lwt_none
        | Some previewed_value -> some <$> submit mode previewed_value
    in
    (
      flip Option.iter result @@ fun result ->
      Component.clear editor;
      match on_save with
      | None ->
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
      | Some on_save -> on_save result
    );
    lwt_unit
  in

  (* Make a page holding the editor and the appropriate buttons and actions. *)
  let promise =
    Page.make'
      ~title: (lwt @@ "Add a " ^ key)
      ~on_load: (fun () -> Component.focus editor)
      [Component.inner_html editor]
      ~buttons: [
        Utils.Button.clear
          ~onclick: (fun () -> Component.clear editor)
          ();
        Utils.Button.save
          ~disabled: (S.map Result.is_error (Component.signal editor))
          ~onclick: save
          ();
      ]
  in

  (* If there was no initial text, then we connected to the local storage. We
     now enable saving the state of the editor when it changes.. *)
  (
    match mode with
    | QuickCreate _ | Edit _ -> ()
    | CreateWithLocalStorage ->
      let store = write_local_storage ~key editor_s in
      let iter = S.map store @@ Component.raw_signal editor in
      (* NOTE: Depending on the promise breaks eventually. Depending on the editor
        seems to live as long as the page lives. *)
      Depart.depends ~on: editor iter
  );

  (* Return the promise of a page. *)
  promise
