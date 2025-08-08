open Nes
open Common

open Components
open Html
open Utils

let editor =
  let open Editor in
  Star.prepare_non_empty
    (
      Input.prepare
        ~type_: Text
        ~label: "Name"
        ~placeholder: "eg. The Dusty Miller"
        ~validator: (S.const % Result.of_string_nonempty ~empty: "The name cannot be empty.")
        ()
    ) ^::
  Input.prepare
    ~type_: Text
    ~label: "Kind"
    ~placeholder: "eg. 8x32R or 2x(16R+16S)"
    ~validator: (
      S.const %
        Option.to_result ~none: "Enter a valid kind, eg. 8x32R or 2x(16R+16S)." %
        Kind.Dance.of_string_opt
    )
    () ^::
  Star.prepare
    (
      Selector.prepare
        ~label: "Deviser"
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Person.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Person Search) slice filter
        )
        ~serialise: Entry.id
        ~unserialise: Model.Person.get
        ~make_result: AnyResult.make_person_result'
        ~model_name: "person"
        ~create_dialog_content: (fun ?on_save text -> PersonEditor.create ?on_save ~text ())
        ()
    ) ^::
  Input.prepare
    ~type_: Text
    ~label: "Date of devising"
    ~placeholder: "eg. 2019 or 2012-03-14"
    ~validator: (
      S.const %
        Option.fold
          ~none: (Ok None)
          ~some: (Result.map some % Option.to_result ~none: "Enter a valid date, eg. 2019, 2015-10, or 2012-03-14." % PartialDate.from_string) %
        Option.of_string_nonempty
    )
    () ^::
  Input.prepare
    ~type_: Text
    ~label: "Disambiguation"
    ~placeholder: "If there are multiple dances with the same name, this field must be used to distinguish them."
    ~validator: (S.const % ok % Option.of_string_nonempty)
    () ^::
  Choices.prepare_radios'
    ~label: "Number of chords"
    ~validate: ok
    [
      Choices.choice' [txt "I don't know"] ~checked: true;
      Choices.choice' ~value: false [txt "One chord"];
      Choices.choice' ~value: true [txt "Two chords"];
    ] ^::
  Input.prepare
    ~type_: Text
    ~label: "SCDDB ID"
    ~placeholder: "eg. 14298 or https://my.strathspey.org/dd/dance/14298/"
    ~validator: (
      S.const %
        Option.fold
          ~none: (Ok None)
          ~some: (Result.map some % SCDDB.entry_from_string SCDDB.Dance) %
        Option.of_string_nonempty
    )
    () ^::
  nil

let create ?on_save ?text () =
  MainPage.assert_can_create @@ fun () ->
  Editor.make_page
    ~key: "dance"
    ~icon: "person-arms-up"
    ?on_save
    ?initial_text: text
    editor
    ~format: (Formatters.Dance.name' ~link: true)
    ~href: (Endpoints.Page.href_dance % Entry.id)
    ~submit: (fun (names, (kind, (devisers, (date, (disambiguation, (two_chords, (scddb_id, ()))))))) ->
      some
      <$> Madge_client.call_exn Endpoints.Api.(route @@ Dance Create) @@
          Model.Dance.make
            ~names
            ~kind
            ~devisers
            ?two_chords
            ?scddb_id
            ?disambiguation
            ?date
            ()
    )
