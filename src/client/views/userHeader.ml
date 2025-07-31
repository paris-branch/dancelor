open Nes
open Common
open Html

type sign_in_dialog_status = DontKnow | Invalid

let open_sign_in_dialog () =
  let open Components in
  let (status_signal, set_status_signal) = S.create DontKnow in
  let username_input =
    Input.Text.make'
      ~type_: Text
      ~label: "Username"
      ~initial_value: ""
      ~placeholder: "JeanMilligan"
      ~oninput: (fun _ -> set_status_signal DontKnow)
      ~validator: (fun username ->
        S.bind status_signal @@ fun status ->
        S.const @@
          if username = "" then Error "The username cannot be empty."
          else
            match status with
            | Invalid -> Error "Invalid username or password."
            | DontKnow -> Ok username
      )
      ()
  in
  let password_input =
    Input.Text.make'
      ~type_: Password
      ~label: "Password"
      ~initial_value: ""
      ~placeholder: "1234567"
      ~oninput: (fun _ -> set_status_signal DontKnow)
      ~validator: (fun password ->
        S.bind status_signal @@ fun status ->
        S.const @@
          match password, status with
          | "", _ -> Error "The password cannot be empty."
          | _, Invalid -> Error "Invalid username or password."
          | _, DontKnow -> Ok password
      )
      ()
  in
  let remember_me_input =
    Choices.(
      make_radios'
        ~name: "Sign in..."
        ~validate: (Option.to_result ~none: "You must make a choice.")
        [
          choice' [txt "Just this once"] ~value: false ~checked: true;
          choice' [txt "Remember me"] ~value: true;
        ]
    )
  in
  let request_signal =
    S.map Result.to_option @@
    RS.bind (Input.Text.signal username_input) @@ fun username ->
    RS.bind (Input.Text.signal password_input) @@ fun password ->
    RS.bind (Choices.signal remember_me_input) @@ fun remember_me ->
    RS.pure (username, password, remember_me)
  in
  let%lwt _ =
    Page.open_dialog @@ fun return ->
    Page.make'
      ~title: (lwt "Sign in")
      [Input.Text.html username_input;
      Input.Text.html password_input;
      Choices.render remember_me_input;
      ]
      ~buttons: [
        Button.cancel' ~return ();
        Button.make
          ~label: "Sign in"
          ~label_processing: "Signing in..."
          ~icon: "box-arrow-in-right"
          ~classes: ["btn-primary"]
          ~disabled: (S.map Option.is_none request_signal)
          ~onclick: (fun () ->
            Option.fold
              (S.value request_signal)
              ~none: lwt_unit
              ~some: (fun (username, password, remember_me) ->
                set_status_signal DontKnow;
                match%lwt Madge_client.call_exn Endpoints.Api.(route @@ User SignIn) username password remember_me with
                | None -> set_status_signal Invalid; lwt_unit
                | Some _ -> return (Some ()); lwt_unit
              )
          )
          ();
      ]
  in
  Js_of_ocaml.Dom_html.window##.location##reload;
  lwt_unit

let sign_out () =
  Madge_client.call_exn Endpoints.Api.(route @@ User SignOut);%lwt
  Js_of_ocaml.Dom_html.window##.location##reload;
  lwt_unit

let rec ping_until_success () =
  let delay = (* every two seconds *) 2. in
  let ping_promise =
    match%lwt Madge_client.call Endpoints.Api.(route Ping) with
    | Ok() -> lwt_true
    | _ -> lwt_false
  in
  let wait_promise = Js_of_ocaml_lwt.Lwt_js.sleep delay;%lwt lwt_false in
  match%lwt Lwt.pick [ping_promise; wait_promise] with
  | true -> lwt_unit
  | false -> ping_until_success ()

let victorise () =
  Lwt.async (fun () ->
    try%lwt
      ignore <$> Madge_client.call ~retry: false Endpoints.Api.(route Victor)
    with
      | Madge_client.(Error (ServerUnreachable _)) -> lwt_unit
  );
  ignore
  <$> Page.open_dialog @@ fun return ->
    Lwt.async (fun () ->
      ping_until_success ();%lwt
      return (Some ());
      Lwt.async (fun () ->
        Js_of_ocaml_lwt.Lwt_js.sleep 2.;%lwt
        Js_of_ocaml.Dom_html.window##.location##reload;
        lwt_unit
      );
      ignore
      <$> Page.open_dialog @@ fun return ->
        Page.make'
          ~title: (lwt "Victorisation")
          [txt "Victorisation successful! The page will now reload."]
          ~buttons: [Components.Button.ok' ~return ()]
    );
    Page.make'
      ~title: (lwt "Victorisation")
      [txt
        "Victorisation in progress. Please wait. The page will reload when \
        Dancelor is ready."]
      ~buttons: [Components.Button.ok' ~return ()]

let header_item =
  let status_lwt = Madge_client.call_exn Endpoints.Api.(route @@ User Status) in
  R.li
    ~a: [
      R.a_class
        (
          S.from' ["nav-item"] @@
          flip Lwt.map status_lwt @@ function
          | None -> ["nav-item"]
          | Some _ -> ["nav-item"; "dropdown"]
        );
    ]
    (
      S.from' [
        Components.Button.make
          ~label: "Sign in"
          ~icon: "box-arrow-in-right"
          ~classes: ["btn-primary"; "disabled"; "placeholder"]
          ()
      ] @@
      flip Lwt.map status_lwt @@ function
      | None ->
        [
          Components.Button.make
            ~label: "Sign in"
            ~icon: "box-arrow-in-right"
            ~classes: ["btn-primary"]
            ~onclick: open_sign_in_dialog
            ()
        ]
      | Some user ->
        [
          Components.Button.make
            ~label: (Model.User.username' user)
            ~icon: "person-circle"
            ~classes: ["btn-primary"; "dropdown-toggle"]
            ~more_a: [a_user_data "bs-toggle" "dropdown"; a_aria "expanded" ["false"]]
            ();
          ul
            ~a: [a_class ["dropdown-menu"]]
            (
              List.flatten
                [
                  (
                    if Model.User.admin user then
                      [
                        li [
                          Components.Button.make_a
                            ~label: "Create user"
                            ~icon: "plus-circle"
                            ~classes: ["dropdown-item"]
                            ~href: (S.const @@ Endpoints.Page.(href UserCreate))
                            ()
                        ];
                        li [
                          Components.Button.make
                            ~label: "Victorise"
                            ~icon: "stop-circle"
                            ~classes: ["dropdown-item"]
                            ~onclick: victorise
                            ()
                        ];
                        li [hr ~a: [a_class ["dropdown-divider"]] ()];
                      ]
                    else []
                  );
                  [li [
                    Components.Button.make_a
                      ~label: "My person"
                      ~icon: "person"
                      ~classes: ["dropdown-item"]
                      ~href: (S.from' "" (Endpoints.Page.(href Person None) <$> (Entry.id <$> Model.User.person' user)))
                      ()
                  ];
                  li [
                    Components.Button.make
                      ~label: "Sign out"
                      ~icon: "box-arrow-right"
                      ~classes: ["dropdown-item"]
                      ~onclick: sign_out
                      ()
                  ];
                  ];
                ]
            );
        ]
    )
