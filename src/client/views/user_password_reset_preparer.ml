open Nes
open Common
open Components
open Html
open Utils

let open_token_result_dialog user token =
  ignore
  <$> Page.open_dialog @@ fun return ->
    Page.make'
      ~title: (lwt "Password reset link generated")
      [p [
        txt "Password reset link for user ";
        txt (Model.User.Username.to_string @@ Model.User.username' user);
        txt " has been generated. Pass them the following link: ";
      ];
      p [
        let href = Endpoints.Page.(href User_password_reset) (Model.User.username' user) token in
        a ~a: [a_href href] [txt href]
      ];
      p [
        txt " for them to create a new password. Note that their existing password and all active sessions have been invalidated.";
      ];
      ]
      ~buttons: [Button.ok' ~return ()]

let create () =
  Main_page.assert_can_admin @@ fun () ->
  let%lwt user_selector =
    Selector.make
      ~label: "User"
      ~model_name: "user"
      ~make_descr: (lwt % Model.User.Username.to_string % Model.User.username')
      ~make_result: (Any_result.make_user_result ?context: None)
      ~results_when_no_search: lwt_nil
      ~search: (fun slice input ->
        let%rlwt filter = lwt (Filter.User.from_string input) in
        ok <$> Madge_client.call_exn Endpoints.Api.(route @@ User Search) slice filter
      )
      ~unserialise: Model.User.get
      None
  in
  let signal = Component.signal user_selector in
  Page.make'
    ~title: (lwt "Reset user password")
    [Component.html user_selector;
    ]
    ~buttons: [
      Button.make
        ~label: "Reset password"
        ~label_processing: "Resetting password..."
        ~classes: ["btn-warning"]
        ~disabled: (S.map Result.is_error signal)
        ~onclick: (fun () ->
          let user = Result.get_ok @@ S.value signal in
          let username = Model.User.username' user in
          let%lwt token = Madge_client.call_exn Endpoints.Api.(route @@ User Prepare_reset_password) username in
          open_token_result_dialog user token
        )
        ();
    ]
