open Nes
open Common
open Components
open Js_of_ocaml
open Html

type status = Match | DontMatch

let create username token =
  let password1_input =
    Input.Text.make
      ~type_: Password
      ~label: "Password"
      ~initial_value: ""
      ~placeholder: "1234567"
      ~validator: (fun password1 -> Result.bind (Password.check password1) @@ fun () -> Ok password1)
      ()
  in
  let password2_input =
    Input.Text.make'
      ~type_: Password
      ~label: "Password, again"
      ~initial_value: ""
      ~placeholder: "1234678"
      ~validator: (fun password2 ->
        flip S.map (Input.Text.raw_signal password1_input) @@ fun password1 ->
        if password1 = password2 then Ok password2 else Error "The passwords do not match."
      )
      ()
  in
  let password =
    RS.bind (Input.Text.signal password1_input) @@ fun password1 ->
    RS.bind (Input.Text.signal password2_input) @@ fun password2 ->
    (* Cannot hurt to check twice. *)
    S.const @@ if password1 = password2 then Ok password2 else Error "The passwords do not match."
  in
  Page.make'
    ~title: (lwt "Reset password")
    [Input.inactive ~label: "Username" username;
    Input.Text.html password1_input;
    Input.Text.html password2_input;
    ]
    ~buttons: [
      Button.make
        ~label: "Reset password"
        ~label_processing: "Resetting password..."
        ~classes: ["btn-primary"]
        ~disabled: (S.map Result.is_error password)
        ~onclick: (fun () ->
          Madge_client.call_exn Endpoints.Api.(route @@ User ResetPassword) username token (Result.get_ok @@ S.value password);%lwt
          Components.Toast.open_ ~title: "Password reset" [txt "Your password has been reset successfully. You may now try to sign in."];
          Dom_html.window##.history##replaceState "fixme-the-state" (Js.string "") (Js.some (Js.string "/"));
          MainPage.load_sleep_raise (Index.create ());%lwt
          lwt_unit
        )
        ();
    ]
