open Nes
open Common
open Html

type sign_in_dialog_status = DontKnow | Invalid

let open_sign_in_dialog () =
  let open Components in
  let (status_signal, set_status_signal) = S.create DontKnow in
  let username_input =
    Input.Text.make' "" (fun username ->
      S.bind status_signal @@ fun status ->
      S.const @@
        if username = "" then Error "The username cannot be empty."
        else
          match status with
          | Invalid -> Error "Invalid username or password."
          | DontKnow -> Ok (Slug.from_string username)
    )
  in
  let password_input =
    Input.Text.make' "" (fun password ->
      S.bind status_signal @@ fun status ->
      S.const @@
        match password, status with
        | "", _ -> Error "The password cannot be empty."
        | _, Invalid -> Error "Invalid username or password."
        | _, DontKnow -> Ok password
    )
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
      [Input.Text.render
        username_input
        ~placeholder: "JeanMilligan"
        ~label: "Username"
        ~oninput: (fun _ -> set_status_signal DontKnow);
      Input.Text.render
        password_input
        ~password: true
        ~placeholder: "1234567"
        ~label: "Password"
        ~oninput: (fun _ -> set_status_signal DontKnow);
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
            ~label: (Model.User.display_name' user)
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
                          Components.Button.make_a
                            ~label: "Victorise"
                            ~icon: "stop-circle"
                            ~classes: ["dropdown-item"]
                            ~href: (S.const @@ Endpoints.Api.(href Victor))
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
                      ~href: (S.from' "" (Endpoints.Page.(href Person None) <$> (Entry.slug <$> Model.User.person' user)))
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
