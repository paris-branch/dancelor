open Nes
open Common
open Components
open Js_of_ocaml
open Html

type status = Match | DontMatch

let create username token =
  let (password1, set_password1) = S.create "" in
  let password1_input =
    Input.Text.make' "" (fun password1 ->
      S.const @@ Result.bind (Password.check password1) @@ fun () -> Ok password1
    )
  in
  let password2_input =
    Input.Text.make' "" (fun password2 ->
      Fun.flip S.map password1 @@ fun password1 ->
      if password1 = password2 then Ok password2 else Error "The passwords do not match."
    )
  in
  let password =
    RS.bind (Input.Text.signal password1_input) @@ fun password1 ->
    RS.bind (Input.Text.signal password2_input) @@ fun password2 ->
    (* The input components already check the password *)
    ignore password1; S.const (Ok password2)
  in
  Page.make
    ~title: (S.const "Reset password")
    [Input.inactive
      ~label: "Username"
      (Slug.to_string username);
    Input.Text.render
      password1_input
      ~password: true
      ~placeholder: "1234567"
      ~label: "Password"
      ~oninput: set_password1;
    Input.Text.render
      password2_input
      ~password: true
      ~placeholder: "1234678"
      ~label: "Password, again";
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
          Lwt.return_unit
        )
        ();
    ]
