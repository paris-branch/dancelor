open Nes
open Common
open Html

type sign_in_dialog_status = Dont_know | Invalid

let open_sign_in_dialog () =
  let open Components in
  let (status_signal, set_status_signal) = S.create Dont_know in
  let%lwt username_input =
    Input.make
      ~type_: Text
      ~label: "Username"
      ~placeholder: "JeanMilligan"
      ~oninput: (fun _ -> set_status_signal Dont_know)
      ~serialise: Fun.id
      ~validate: (fun username ->
        S.bind status_signal @@ fun status ->
        S.const @@
          if username = "" then Error "The username cannot be empty."
          else
            match status with
            | Invalid -> Error "Invalid username or password."
            | Dont_know -> Ok username
      )
      ""
  in
  let%lwt password_input =
    Input.make
      ~type_: Password
      ~label: "Password"
      ~placeholder: "1234567"
      ~oninput: (fun _ -> set_status_signal Dont_know)
      ~serialise: Fun.id
      ~validate: (fun password ->
        S.bind status_signal @@ fun status ->
        S.const @@
          match password, status with
          | "", _ -> Error "The password cannot be empty."
          | _, Invalid -> Error "Invalid username or password."
          | _, Dont_know -> Ok password
      )
      ""
  in
  let%lwt remember_me_input =
    Choices.(
      make_radios'
        ~label: "Sign in..."
        ~validate: (Option.to_result ~none: "You must make a choice.")
        [
          choice' [txt "Just this once"] ~value: false ~checked: true;
          choice' [txt "Remember me"] ~value: true;
        ]
    )
  in
  let request_signal =
    S.map Result.to_option @@
    RS.bind (Component.signal username_input) @@ fun username ->
    RS.bind (Component.signal password_input) @@ fun password ->
    RS.bind (Component.signal remember_me_input) @@ fun remember_me ->
    RS.pure (username, password, remember_me)
  in
  let%lwt _ =
    Page.open_dialog @@ fun return ->
    Page.make'
      ~title: (lwt "Sign in")
      ~on_load: (fun () -> Component.focus username_input)
      [Component.html username_input;
      Component.html password_input;
      Component.html remember_me_input;
      ]
      ~buttons: [
        Utils.Button.cancel' ~return ();
        Utils.Button.make
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
                set_status_signal Dont_know;
                match%lwt Madge_client.call_exn Endpoints.Api.(route @@ User Sign_in) username password remember_me with
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
  Madge_client.call_exn Endpoints.Api.(route @@ User Sign_out);%lwt
  Js_of_ocaml.Dom_html.window##.location##reload;
  lwt_unit

let rec ping_until_success () =
  let delay = (* every two seconds *) 2. in
  let ping_promise =
    match%lwt Madge_client.call Endpoints.Api.(route Boot_time) with
    | Ok _ -> lwt_true
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
      | Madge_client.(Error (Server_unreachable _)) -> lwt_unit
  );
  Utils.Toast.open_
    ~title: "Victorisation"
    [txt "Victorisation in progress. Please wait."]

let set_omniscience enable =
  let%lwt _ = Madge_client.call Endpoints.Api.(route @@ User Set_omniscience) enable in
  Utils.Toast.open_
    ~type_: Forever
    ~title: "Omniscience"
    [txtf "Omniscience has been %s. You should reload for it to take effect." (if enable then "enabled" else "disabled")]
    ~buttons: [
      Utils.Button.make
        ~label: "Reload"
        ~icon: "arrow-clockwise"
        ~classes: ["btn-primary"]
        ~onclick: (fun () -> Js_of_ocaml.Dom_html.window##.location##reload; lwt_unit)
        ();
    ];
  lwt_unit

let header_item =
  R.li
    ~a: [
      R.a_class
        (
          S.from' ["nav-item"] @@
          flip Lwt.map Environment.user @@ function
          | None -> ["nav-item"]
          | Some _ -> ["nav-item"; "dropdown"]
        );
    ]
    (
      S.from' [
        Utils.Button.make
          ~label: "Sign in"
          ~icon: "box-arrow-in-right"
          ~classes: ["disabled"; "placeholder"]
          ()
      ] @@
      flip Lwt.map Environment.user @@ function
      | None ->
        [
          Utils.Button.make
            ~label: "Sign in"
            ~icon: "box-arrow-in-right"
            ~classes: ["text-white"]
            ~onclick: open_sign_in_dialog
            ()
        ]
      | Some user ->
        [
          Utils.Button.make
            ~label: (NEString.to_string @@ Model.User.username' user)
            ~icon: "person-circle"
            ~classes: ["text-white"; "dropdown-toggle"]
            ~more_a: [a_user_data "bs-toggle" "dropdown"; a_aria "expanded" ["false"]]
            ();
          ul
            ~a: [a_class ["dropdown-menu"]]
            (
              List.flatten
                [
                  (
                    if Model.User.is_administrator' user then
                      [
                        li [
                          Utils.Button.make_a
                            ~label: "Create user"
                            ~icon: "plus-circle"
                            ~dropdown: true
                            ~href: (S.const @@ Endpoints.Page.(href User_create))
                            ()
                        ];
                        li [
                          Utils.Button.make
                            ~label: "Victorise"
                            ~icon: "stop-circle"
                            ~dropdown: true
                            ~onclick: (fun () -> victorise (); lwt_unit)
                            ()
                        ];
                        li [
                          if Model.User.is_omniscient_administrator' user then
                            Utils.Button.make
                              ~label: "Disable omniscience"
                              ~icon: "shield-lock-fill"
                              ~dropdown: true
                              ~onclick: (fun () -> set_omniscience false)
                              ()
                          else
                            Utils.Button.make
                              ~label: "Enable omniscience"
                              ~icon: "shield-lock"
                              ~dropdown: true
                              ~onclick: (fun () -> set_omniscience true)
                              ()
                        ];
                        li [hr ~a: [a_class ["dropdown-divider"]] ()];
                      ]
                    else []
                  );
                  [R.li (
                    S.from' [] @@
                      match%lwt Madge_client.call_exn Endpoints.Api.(route @@ Person For_user) (Entry.id user) with
                      | None -> lwt_nil
                      | Some person ->
                        lwt [
                          Utils.Button.make_a
                            ~label: "My person"
                            ~icon: "person"
                            ~dropdown: true
                            ~href: (S.const (Endpoints.Page.(href Person None) (Entry.id person)))
                            ()
                        ]
                  );
                  li [
                    Utils.Button.make
                      ~label: "Sign out"
                      ~icon: "box-arrow-right"
                      ~dropdown: true
                      ~onclick: sign_out
                      ()
                  ];
                  ];
                ]
            );
        ]
    )
