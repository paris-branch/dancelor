open Nes
open Common

open Components
open Html
open Utils

let editor =
  let open Editor in
  Star.prepare_non_empty
    ~label: "Names"
    (
      Input.prepare_non_empty
        ~type_: Text
        ~label: "Name"
        ~placeholder: "eg. The Dusty Miller"
        ()
    ) ^::
  Input.prepare
    ~type_: Text
    ~label: "Kind"
    ~placeholder: "eg. 8x32R or 2x(16R+16S)"
    ~serialise: Kind.Dance.to_string
    ~validate: (
      S.const %
        Option.to_result ~none: "Enter a valid kind, eg. 8x32R or 2x(16R+16S)." %
        Kind.Dance.of_string_opt
    )
    () ^::
  Star.prepare
    ~label: "Devisers"
    (
      Selector.prepare
        ~label: "Deviser"
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Person.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Person Search) slice filter
        )
        ~unserialise: Model.Person.get
        ~make_descr: (lwt % NEString.to_string % Model.Person.name')
        ~make_result: (Any_result.make_person_result ?context: None)
        ~results_when_no_search: (Option.to_list <$> Environment.person)
        ~model_name: "person"
        ~create_dialog_content: Person_editor.create
        ()
    ) ^::
  Input.prepare_option
    ~type_: Text
    ~label: "Date of devising"
    ~placeholder: "eg. 2019 or 2012-03-14"
    ~serialise: (NEString.of_string_exn % PartialDate.to_string)
    (* FIXME: make PartialDate.to_string return NEString.t *)
    ~validate: (
      S.const %
        Option.to_result ~none: "Enter a valid date, eg. 2019, 2015-10, or 2012-03-14." %
        PartialDate.from_string %
        NEString.to_string
    )
    () ^::
  Input.prepare
    ~type_: Text
    ~label: "Disambiguation"
    ~placeholder: "If there are multiple dances with the same name, this field must be used to distinguish them."
    ~serialise: Fun.id
    ~validate: (S.const % ok)
    () ^::
  Choices.prepare_radios'
    ~label: "Number of chords"
    ~validate: (Option.to_result ~none: "You must select something.")
    [
      Choices.choice ~value: (Some Model.Dance.Dont_know) [txt "I don't know"] ~checked: true;
      Choices.choice ~value: (Some Model.Dance.One_chord) [txt "One chord"];
      Choices.choice ~value: (Some Model.Dance.Two_chords) [txt "Two chords"];
    ] ^::
  Input.prepare
    ~type_: Text
    ~label: "SCDDB ID"
    ~placeholder: "eg. 14298 or https://my.strathspey.org/dd/dance/14298/"
    ~serialise: (Option.fold ~none: "" ~some: string_of_int)
    ~validate: (
      S.const %
        Option.fold
          ~none: (Ok None)
          ~some: (Result.map some % SCDDB.entry_from_string SCDDB.Dance) %
        Option.of_string_nonempty
    )
    () ^::
  nil

let assemble (names, (kind, (devisers, (date, (disambiguation, (two_chords, (scddb_id, ()))))))) =
  Model.Dance.make ~names ~kind ~devisers ~two_chords ?scddb_id ~disambiguation ?date ()

let submit mode dance =
  match mode with
  | Editor.Edit prev_dance -> Madge_client.call_exn Endpoints.Api.(route @@ Dance Update) (Entry.id prev_dance) dance
  | _ -> Madge_client.call_exn Endpoints.Api.(route @@ Dance Create) dance

let unsubmit = lwt % Entry.value

let disassemble dance =
  let names = Model.Dance.names dance in
  let kind = Model.Dance.kind dance in
  let%lwt devisers = Model.Dance.devisers dance in
  let date = Model.Dance.date dance in
  let disambiguation = Model.Dance.disambiguation dance in
  let two_chords = Model.Dance.two_chords dance in
  let scddb_id = Model.Dance.scddb_id dance in
  lwt (names, (kind, (devisers, (date, (disambiguation, (two_chords, (scddb_id, ())))))))

let create mode =
  (* FIXME: if [mode] is an edition, then we should assert_can_update_public *)
  Main_page.assert_can_create_public @@ fun () ->
  Editor.make_page
    ~key: "dance"
    ~icon: (Model Dance)
    ~mode
    editor
    ~format: (Formatters.Dance.name' ~link: true)
    ~href: (Endpoints.Page.href_dance % Entry.id)
    ~assemble
    ~submit
    ~unsubmit
    ~disassemble
    ~check_product: Model.Dance.equal
