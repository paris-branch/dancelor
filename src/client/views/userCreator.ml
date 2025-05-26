open Nes
open Common
open Components
open Html

type status = Match | DontMatch

let open_token_result_dialog user token =
  Lwt.map ignore @@
  Page.open_dialog @@ fun return ->
  Page.make'
    ~title: (Lwt.return "Created user")
    [p [
      txt "User ";
      txt (Entry.slug_as_string user);
      txt " was created successfully. Pass them the following link: ";
    ];
    p [
      let href = Endpoints.Page.(href UserPasswordReset) (Entry.slug user) token in
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
    Input.Text.make "" (fun username ->
      if username = "" then Error "The username cannot be empty."
      else
        match Slug.check_string username with
        | None -> Error "This does not look like a username."
        | Some username -> Ok username
    )
  in
  let display_name_input =
    Input.Text.make' "" (fun display_name ->
      Fun.flip S.map (Input.Text.raw_signal username_input) @@ fun username ->
      if String.slugify display_name <> username then
        Error "The display name must correspond to the username."
      else
        Ok display_name
    )
  in
  let person_selector =
    Selector.make
      ~arity: Selector.one
      ~search: (fun slice input ->
        let%rlwt filter = Lwt.return (Filter.Person.from_string input) in
        Lwt.map Result.ok @@
          Madge_client.call_exn
            Endpoints.Api.(route @@ Person Search)
            slice
            filter
      )
      ~serialise: Entry.slug
      ~unserialise: Model.Person.get
      []
  in
  let signal =
    RS.bind (Input.Text.signal username_input) @@ fun username ->
    RS.bind (Input.Text.signal display_name_input) @@ fun display_name ->
    RS.bind (Selector.signal_one person_selector) @@ fun person ->
    S.const @@ Ok (username, Model.User.make ~display_name ~person ())
  in
  Page.make'
    ~title: (Lwt.return "Create user")
    [Input.Text.render
      username_input
      ~placeholder: "jeanmilligan"
      ~label: "Username";
    Input.Text.render
      display_name_input
      ~placeholder: "JeanMilligan"
      ~label: "Display name";
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
          let (username, user) = Result.get_ok @@ S.value signal in
          let%lwt (user, token) = Madge_client.call_exn Endpoints.Api.(route @@ User Create) username user in
          open_token_result_dialog user token;%lwt
          Input.Text.clear username_input;
          Input.Text.clear display_name_input;
          Selector.clear person_selector;
          Lwt.return_unit
        )
        ();
    ]
