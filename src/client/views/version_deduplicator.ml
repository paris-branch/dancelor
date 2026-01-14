open Nes
open Common
open Model
open Html
open Utils

let show_lilypond_dialog version =
  let content_promise =
    let%lwt content = Madge_client.call_exn Endpoints.Api.(route @@ Version Content) (Entry.id version) in
    let content =
      match content with
      | Endpoints.Version.Protected -> assert false
      | Endpoints.Version.Granted {payload; _} -> payload
    in
    Model.Version.content_lilypond' ~content version
  in
  ignore
  <$> Page.open_dialog @@ fun return ->
    Page.make'
      ~title: (lwt "LilyPond")
      [with_div_placeholder (
        let%lwt content = content_promise in
        lwt [pre [txt content]]
      )]
      ~buttons: [
        Button.close' ~return ();
        Button.make
          ~label: "Copy to clipboard"
          ~icon: (Other Clipboard)
          ~classes: ["btn-primary"]
          ~onclick: (fun _ ->
            let%lwt content = content_promise in
            write_to_clipboard content;
            Toast.open_ ~title: "Copied to clipboard" [txt "The LilyPond content was copied to your clipboard."];
            return (some ());
            lwt_unit
          )
          ()
      ]

(* helpers to record all the changes that will be performed and spit them back
   to the user afterwards *)
let make_change_trackers () =
  let changes = ref [] in
  (
    (fun () -> List.rev_map fst !changes),
    (fun () -> List.rev_map snd !changes),
    (fun ?(action = fun () -> lwt_unit) html ->
      changes := (action, html) :: !changes
    )
  )

(* /!\ deduplicate this version INTO the other version *)
let confirmation_dialog ~this_version ~other_version =
  let (get_changes_actions, get_changes_html, add_changes) = make_change_trackers () in

  (* changes to other version *)
  let%lwt () =
    let (_, get_other_version_changes, add_other_version_changes) =
      make_change_trackers ()
    in
    (* tune *)
    let%lwt this_tune = Model.Version.tune' this_version in
    let%lwt other_tune = Model.Version.tune' other_version in
    if not (Entry.equal' this_tune other_tune) then
      failwith "Version de-duplicator: these two versions do not share the same tune.";
    (* key *)
    let this_key = Model.Version.key' this_version in
    let other_key = Model.Version.key' other_version in
    if this_key <> other_key then
      failwith "Version de-duplicator: these two versions do not share the same key.";
    (* FIXME: can do better? *)
    (* sources *)
    let%lwt this_sources = Model.Version.sources' this_version in
    let%lwt other_sources = Model.Version.sources' other_version in
    let other_sources = other_sources @ this_sources in
    (* FIXME: de-duplicate and detect the changes? *)
    (
      match this_sources with
      | [] -> ()
      | _ ->
        add_other_version_changes [
          txt "add the following sources:";
          ul (
            List.map
              (fun Model.Version.{source; structure; _} ->
                li [
                  Formatters.Source.name' source;
                  txtf " (%s)" (NEString.to_string @@ Version.Structure.to_string structure);
                ]
              )
              this_sources
          );
        ]
    );
    (* arrangers *)
    let%lwt this_arrangers = Model.Version.arrangers' this_version in
    let%lwt other_arrangers = Model.Version.arrangers' other_version in
    if this_arrangers <> other_arrangers then
      failwith "Version de-duplicator: these two versions do not share the same arrangers.";
    (* FIXME: can do better? *)
    (* remark *)
    let this_remark = Model.Version.remark' this_version in
    let other_remark = Model.Version.remark' other_version in
    if this_remark <> other_remark then
      failwith "Version de-duplicator: these two versions do not share the same remark.";
    (* FIXME: can do better? *)
    (* disambiguation *)
    let this_disambiguation = Model.Version.disambiguation' this_version in
    let other_disambiguation = Model.Version.disambiguation' other_version in
    if this_disambiguation <> other_disambiguation then
      failwith "Version de-duplicator: these two versions do not share the same disambiguation.";
    (* FIXME: can do better? *)
    (* content *)
    let%lwt other_content = Madge_client.call_exn Endpoints.Api.(route @@ Version Content) (Entry.id other_version) in
    let other_content =
      match other_content with
      | Endpoints.Version.Protected -> assert false
      | Endpoints.Version.Granted {payload; _} -> payload
    in
    add_other_version_changes [txt "use its content; the content of the current version will be lost entirely."];
    (* that's it for changes to the other version; bundle them together as a change *)
    let other_version_formatted =
      span [
        Formatters.Version.name_disambiguation_and_sources' other_version;
        txt " [";
        Formatters.Version.id' other_version;
        txt "]"
      ]
    in
    (
      match get_other_version_changes () with
      | [] ->
        add_changes [txt "This will not change anything to the other version, "; other_version_formatted; txt "."];
      | changes ->
        add_changes
          ~action: (fun () ->
            (* FIXME: it would in fact be better if [make] didn't have any
               optional arguments. This would ensure that we cannot forget
               things. We use it so rarely anyway that the convenience isn't
               really anything we care about. *)
            ignore
            <$> Madge_client.call_exn
                Endpoints.Api.(route @@ Version Update)
                (Entry.id other_version) @@
                Model.Version.make
                  ~tune: other_tune
                  ~key: other_key
                  ~sources: other_sources
                  ~arrangers: other_arrangers
                  ~remark: other_remark
                  ~disambiguation: other_disambiguation
                  ~content: other_content
                  ()
          (* FIXME: we should report nicely if things fail *)
          )
          [
            txt "update the other version, ";
            other_version_formatted;
            txt ", in the following way:";
            ul (List.map li changes);
          ]
    );
    lwt_unit
  in

  (* how to update a version and its params from a set or a book *)
  let replace_version_and_params (a_version, a_version_params) =
    if Entry.equal' a_version this_version then
      (
        other_version,
        Model.Version_parameters.compose
          (
            (* if [this_version] is monolithic, then it comes
               with a specific structure, and this information
               was used when making the set, so we keep it as a
               version parameter to the updated set *)
            match Model.Version.content' this_version with
            | Monolithic {structure; _} -> Model.Version_parameters.make ~structure ()
            | _ -> Model.Version_parameters.none
          )
          a_version_params (* existing parameters take precedence *)
      )
    else (a_version, a_version_params)
  in

  (* changes to sets *)
  let%lwt sets =
    snd
    <$> Madge_client.call_exn
        Endpoints.Api.(route @@ Set Search)
        Slice.everything
        (Filter.(Set.memversion') this_version)
  in
  List.iter
    (fun set ->
      add_changes
        ~action: (fun () ->
          let%lwt contents = Model.Set.contents' set in
          let contents = List.map replace_version_and_params contents in
          ignore
          <$> Madge_client.call_exn
              Endpoints.Api.(route @@ Set Update)
              (Entry.id set)
              (Model.Set.set_contents contents (Entry.value set))
              (Entry.access set)
        )
        [txt "replace the version in set "; Formatters.Set.name' set; txt "."]
    )
    sets;

  (* changes to books *)
  let%lwt books =
    snd
    <$> Madge_client.call_exn
        Endpoints.Api.(route @@ Book Search)
        Slice.everything
        (Filter.(Book.memversion') this_version)
  in
  List.iter
    (fun book ->
      add_changes
        ~action: (fun () ->
          let%lwt contents = Model.Book.contents' book in
          let contents =
            List.map
              (function
                | Model.Book.Dance (dance, Dance_versions versions_and_params) ->
                  Model.Book.Dance (dance, Model.Book.Dance_versions (NEList.map replace_version_and_params versions_and_params))
                | Model.Book.Versions versions_and_params ->
                  Model.Book.Versions (NEList.map replace_version_and_params versions_and_params)
                | page -> page
              )
              contents
          in
          ignore
          <$> Madge_client.call_exn
              Endpoints.Api.(route @@ Book Update)
              (Entry.id book)
              (Model.Book.set_contents contents (Entry.value book))
              (Entry.access book)
        )
        [
          txt "replace the version in book ";
          Formatters.Book.title' book;
          txt "."
        ]
    )
    books;

  (* removal of the current version *)
  add_changes
    ~action: (fun () ->
      ignore
      <$> Madge_client.call_exn Endpoints.Api.(route @@ Version Delete) (Entry.id this_version)
    )
    [
      txt "delete the current version, ";
      Formatters.Version.name_disambiguation_and_sources' this_version;
      txt " [";
      Formatters.Version.id' this_version;
      txt "].";
    ];

  (* report *)
  let%lwt user_input =
    Page.open_dialog @@ fun return ->
    Page.make'
      ~title: (lwt "De-duplicate a version")
      [Alert.make ~level: Warning [txt "This action is irreversible."];
      div ~a: [a_class ["mt-4"]] [txt "This will:"; ul (List.map li (get_changes_html ()))]]
      ~buttons: [
        Button.cancel' ~return ();
        Button.make
          ~classes: ["btn-primary"]
          ~label: "Proceed"
          ~label_processing: "Proceeding"
          ~icon: (Action Deduplicate)
          ~onclick: (lwt % return % some)
          ();
      ]
  in
  (* let's go *)
  match user_input with
  | None -> lwt_unit
  | Some() ->
    Toast.open_
      ~title: "De-duplicating a version"
      [
        txt
          "Dancelor has started de-duplicating this version. Hopefully, you see \
           no error, and another toast comes to announce the good news! \
           Otherwise, please report immediately to a system administrator, \
           because the database might be in an odd state.";
      ];
    Lwt_list.iter_s (fun action -> action ()) (get_changes_actions ());%lwt
    Toast.open_
      ~type_: Forever
      ~title: "De-duplicated a version"
      [txt
        "The version has been de-duplicated successfully! This means that \
           you are on a page that does not exist anymore. Run, you fools!"]
      ~buttons: [
        Button.make_a
          ~label: "Go to other version"
          ~icon: (Model Version)
          ~classes: ["btn-primary"]
          ~href: (S.const @@ Endpoints.Page.href_version (Model_builder.Core.Version.tune' other_version) (Entry.id other_version))
          ();
      ];
    lwt_unit

let dialog ~version ~other_versions_promise () =
  let%lwt other_versions = other_versions_promise in
  ignore
  <$> Page.open_dialog @@ fun return ->
    Page.make'
      ~title: (lwt "De-duplicate a version")
      [txt
        "Only do this if the two versions are actually the same, or if the other \
         one is a destructured version that can encompass this one.";
      Tables.versions
        other_versions
        ~onclick: (fun other_version ->
          confirmation_dialog ~this_version: version ~other_version;%lwt
          return (some ());
          lwt_unit
        );
      ]
      ~buttons: [
        Button.close' ~return ();
      ]
