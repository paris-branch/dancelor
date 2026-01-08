open Nes
open Common
open Components
open Js_of_ocaml
open Utils
open Html

type status = Match | Dont_match

let create username token =
  let%lwt password1_input =
    Input.make
      ~type_: Password
      ~label: "Password"
      ~placeholder: "1234567"
      ~serialise: Fun.id
      ~validate: (S.const % fun password1 -> Result.bind (Password.check password1) @@ fun () -> Ok password1)
      ""
  in
  let%lwt password2_input =
    Input.make
      ~type_: Password
      ~label: "Password, again"
      ~placeholder: "1234678"
      ~serialise: Fun.id
      ~validate: (fun password2 ->
        flip S.map (Component.state password1_input) @@ fun password1 ->
        if password1 = password2 then Ok password2 else Error "The passwords do not match."
      )
      ""
  in
  let password =
    RS.bind (Component.signal password1_input) @@ fun password1 ->
    RS.bind (Component.signal password2_input) @@ fun password2 ->
    (* Cannot hurt to check twice. *)
    S.const @@ if password1 = password2 then Ok password2 else Error "The passwords do not match."
  in
  Page.make'
    ~title: (lwt "Reset password")
    [Input.inactive ~label: "Username" username;
    Component.html password1_input;
    Component.html password2_input;
    ]
    ~buttons: [
      Button.make
        ~label: "Reset password"
        ~label_processing: "Resetting password..."
        ~classes: ["btn-primary"]
        ~disabled: (S.map Result.is_error password)
        ~onclick: (fun () ->
          Madge_client.call_exn Endpoints.Api.(route @@ User Reset_password) username token (Result.get_ok @@ S.value password);%lwt
          Toast.open_ ~title: "Password reset" [txt "Your password has been reset successfully. You may now try to sign in."];
          Dom_html.window##.history##replaceState "fixme-the-state" (Js.string "") (Js.some (Js.string "/"));
          Main_page.load_sleep_raise (Index.create ());%lwt
          lwt_unit
        )
        ();
    ]
