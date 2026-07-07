open Nes
open Dancelor_common
open Model_new
open Search_new
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
        txt (Username.to_string user.User_row.username);
        txt " has been generated. Pass them the following link: ";
      ];
      p [
        let href = Endpoints.Page.(href @@ User Password_reset) user.username token in
        a ~a: [a_href href] [txt @@ Uri.to_string href]
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
      ~make_descr: (fun user -> lwt @@ Username.to_string user.username)
      ~make_result: (Any_result_new.make_user_result ?in_search: None)
      ~results_when_no_search: lwt_nil
      ~search: (fun slice input ->
        match User_query.parse input with
        | Error msg -> lwt_error msg
        | Ok query -> ok <$> Madge_client.call_exn Endpoints.Api.(route @@ User Search) slice query
      )
      ~id_to_yojson: Entry.Id.to_yojson'
      ~id_of_yojson: Entry.Id.of_yojson'
      ~serialise: User_row.id
      ~unserialise: (madge_call_or_option @@ User Get_row)
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
          let%lwt token = Madge_client.call_exn Endpoints.Api.(route @@ User Prepare_reset_password) user.username in
          open_token_result_dialog user token
        )
        ();
    ]
