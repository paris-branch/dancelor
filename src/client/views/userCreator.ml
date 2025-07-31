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
        let href = Endpoints.Page.(href UserPasswordReset) (Model.User.username' user) token in
        a ~a: [a_href href] [txt href]
      ];
      p [
        txt " for them to create a password.";
      ];
      ]
      ~buttons: [Button.ok' ~return ()]

let create () =
  MainPage.assert_can_admin @@ fun () ->
  let username_input =
    Input.make
      ~type_: Text
      ~initial_value: ""
      ~placeholder: "JeanMilligan"
      ~label: "Username"
      ~validator: (fun username ->
        if username = "" then Error "The username cannot be empty."
        else Ok username (* FIXME: limit possibilities? FIXME: a module for usernames *)
      )
      ()
  in
  let person_selector =
    Selector.make
      ~arity: Selector.one
      ~search: (fun slice input ->
        let%rlwt filter = lwt (Filter.Person.from_string input) in
        ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Person Search) slice filter
      )
      ~serialise: Entry.id
      ~unserialise: Model.Person.get
      []
  in
  let signal =
    RS.bind (Input.signal username_input) @@ fun username ->
    RS.bind (Selector.signal_one person_selector) @@ fun person ->
    S.const @@ Ok (Model.User.make ~username ~person ())
  in
  Page.make'
    ~title: (lwt "Create user")
    [Input.html username_input;
    Selector.render
      ~make_result: Utils.AnyResult.make_person_result'
      ~field_name: "Person"
      ~model_name: "person"
      ~create_dialog_content: (fun ?on_save text -> PersonEditor.create ?on_save ~text ())
      person_selector;
    ]
    ~buttons: [
      Button.make
        ~label: "Create user"
        ~label_processing: "Creating user..."
        ~classes: ["btn-primary"]
        ~disabled: (S.map Result.is_error signal)
        ~onclick: (fun () ->
          let user = Result.get_ok @@ S.value signal in
          let%lwt (user, token) = Madge_client.call_exn Endpoints.Api.(route @@ User Create) user in
          open_token_result_dialog user token;%lwt
          Input.clear username_input;
          Selector.clear person_selector;
          lwt_unit
        )
        ();
    ]
