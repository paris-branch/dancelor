open Nes
open Common
open Html

type login_dialog_status = DontKnow | Invalid

let open_login_dialog () =
  let open Components in
  let (status_signal, set_status_signal) = S.create DontKnow in
  let username_input =
    Input.Text.make' "" (fun username ->
      S.bind status_signal @@ fun status ->
      S.const @@
        match username, status with
        | "", _ -> Error "The username cannot be empty."
        | _, Invalid -> Error "Invalid username or password."
        | _, DontKnow -> Ok username
    )
  in
  let password_input =
    (* FIXME: Password field *)
    Input.Text.make' "" (fun password ->
      S.bind status_signal @@ fun status ->
      S.const @@
        match password, status with
        | "", _ -> Error "The password cannot be empty."
        | _, Invalid -> Error "Invalid username or password."
        | _, DontKnow -> Ok password
    )
  in
  let request_signal =
    S.map Result.to_option @@
    RS.bind (Input.Text.signal username_input) @@ fun username ->
    RS.bind (Input.Text.signal password_input) @@ fun password ->
    RS.pure (username, password)
  in
  let%lwt _ =
    Page.open_dialog @@ fun return ->
    Page.make
      ~title: (S.const "Sign in")
      [Input.Text.render
        username_input
        ~placeholder: "jean"
        ~label: "Username"
        ~oninput: (fun _ -> set_status_signal DontKnow);
      Input.Text.render
        password_input
        ~password: true
        ~placeholder: "password"
        ~label: "Password"
        ~oninput: (fun _ -> set_status_signal DontKnow);
      ]
      ~buttons: [
        Button.cancel' ~return ();
        Button.make
          ~label: "Sign in"
          ~label_processing: "Signing in..."
          ~icon: "door-open"
          ~classes: ["btn-primary"]
          ~disabled: (S.map Option.is_none request_signal)
          ~onclick: (fun () ->
            Option.fold
              (S.value request_signal)
              ~none: Lwt.return_unit
              ~some: (fun (username, password) ->
                set_status_signal DontKnow;
                match%lwt Madge_cohttp_lwt_client.call Endpoints.Api.(route @@ Auth Login) username password with
                | None -> set_status_signal Invalid; Lwt.return_unit
                | Some _ -> return (Some ()); Lwt.return_unit
              )
          )
          ();
      ]
  in
  Js_of_ocaml.Dom_html.window##.location##reload;
  Lwt.return_unit

let logout () =
  Madge_cohttp_lwt_client.call Endpoints.Api.(route @@ Auth Logout);%lwt
  Js_of_ocaml.Dom_html.window##.location##reload;
  Lwt.return_unit

let header_item =
  let status_lwt = Madge_cohttp_lwt_client.call Endpoints.Api.(route @@ Auth Status) in
  L.li
    ~a: [
      L.a_class
        (
          Fun.flip Lwt.map status_lwt @@ function
            | None -> ["nav-item"]
            | Some _ -> ["nav-item"; "dropdown"]
        );
    ]
    (
      Fun.flip Lwt.map status_lwt @@ function
        | None ->
          [
            button
              ~a: [
                a_button_type `Button;
                a_class ["btn"; "btn-primary"];
                a_onclick (fun _ev -> Lwt.async open_login_dialog; false);
              ]
              [txt "Login"];
          ]
        | Some person ->
          [
            button
              ~a: [a_button_type `Button; a_class ["btn"; "btn-primary"; "dropdown-toggle"]; a_user_data "bs-toggle" "dropdown"; a_aria "expanded" ["false"]]
              [txt (Model.Person.name person)];
            ul
              ~a: [a_class ["dropdown-menu"]]
              [
                li [
                  a
                    ~a: [
                      a_class ["dropdown-item"];
                      a_onclick (fun _ev -> Lwt.async logout; false);
                    ]
                    [txt "Logout"];
                ];
              ];
          ]
    )
