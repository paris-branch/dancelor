open Nes
open Common
open Components
open Html

type status = Match | DontMatch

let open_token_result_dialog username token =
  Lwt.map ignore @@
  Page.open_dialog @@ fun return ->
  Page.make
    ~title: (S.const "Created user")
    [p [
      txt "User ";
      txt username;
      txt " was created successfully. Pass them the following link: ";
    ];
    p [
      let href = Endpoints.Page.(href AuthPasswordReset) username token in
      a ~a: [a_href href] [txt href]
    ];
    p [
      txt " for them to create a password.";
    ];
    ]
    ~buttons: [Button.ok' ~return ()]

let create () =
  let username_input =
    Input.Text.make "" @@
      Result.of_string_nonempty ~empty: "The username cannot be empty."
  in
  let person_selector =
    Selector.make
      ~arity: Selector.one
      ~search: (fun slice input ->
        let%rlwt filter = Lwt.return (Model.Person.Filter.from_string input) in
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
    RS.bind (Selector.signal_one person_selector) @@ fun person ->
    S.const @@ Ok (username, person)
  in
  Page.make
    ~title: (S.const "Create user")
    [Input.Text.render
      username_input
      ~placeholder: "jeanmilligan"
      ~label: "Username";
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
          let (username, person) = Result.get_ok @@ S.value signal in
          let%lwt token = Madge_client.call_exn Endpoints.Api.(route @@ Auth CreateUser) username (Entry.slug person) in
          open_token_result_dialog username token
        )
        ();
    ]
