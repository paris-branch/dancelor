open Nes
open Common
open Components
open Html

type status = Match | DontMatch

let open_token_result_dialog user token =
  ignore
  <$> Page.open_dialog @@ fun return ->
    Page.make'
      ~title: (lwt "Created user")
      [p [
        txt "User ";
        txt (Entry.id_as_string user);
        txt " was created successfully. Pass them the following link: ";
      ];
      p [
        let href = Endpoints.Page.(href UserPasswordReset) (NEString.to_string @@ Model.User.username' user) token in
        a ~a: [a_href href] [txt href]
      ];
      p [
        txt " for them to create a password.";
      ];
      ]
      ~buttons: [Utils.Button.ok' ~return ()]

let create () =
  MainPage.assert_can_admin @@ fun () ->
  let%lwt username_input =
    Input.make_non_empty
      ~type_: Text
      ~placeholder: "JeanMilligan"
      ~label: "Username"
      ""
  in
  let%lwt person_selector =
    Selector.make
      ~label: "Person"
      ~search: (fun slice input ->
        let%rlwt filter = lwt (Filter.Person.from_string input) in
        ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Person Search) slice filter
      )
      ~unserialise: Model.Person.get
      ~make_descr: (lwt % NEString.to_string % Model.Person.name')
      ~make_result: Utils.AnyResult.make_person_result'
      ~model_name: "person"
      ~create_dialog_content: PersonEditor.create
      None
  in
  let signal =
    RS.bind (Component.signal username_input) @@ fun username ->
    RS.bind (Component.signal person_selector) @@ fun person ->
    S.const @@ Ok (Model.User.make ~username ~person ())
  in
  Page.make'
    ~title: (lwt "Create user")
    [Component.html username_input;
    Component.html person_selector;
    ]
    ~buttons: [
      Utils.Button.make
        ~label: "Create user"
        ~label_processing: "Creating user..."
        ~classes: ["btn-primary"]
        ~disabled: (S.map Result.is_error signal)
        ~onclick: (fun () ->
          let user = Result.get_ok @@ S.value signal in
          let%lwt (user, token) = Madge_client.call_exn Endpoints.Api.(route @@ User Create) user in
          open_token_result_dialog user token;%lwt
          Component.clear username_input;
          Component.clear person_selector;
          lwt_unit
        )
        ();
    ]
