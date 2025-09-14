open Nes
open Common
open Model
open Html

let create ?context id =
  MainPage.madge_call_or_404 (Dance Get) id @@ fun dance ->
  Page.make'
    ~parent_title: "Dance"
    ~before_title: [
      Components.ContextLinks.make_and_render
        ?context
        ~this_page: (Endpoints.Page.href_dance id)
        (lwt @@ Any.dance dance);
    ]
    ~title: (lwt @@ NEString.to_string @@ Dance.one_name' dance)
    ~subtitles: [
      Formatters.Dance.aka' dance;
      (
        with_span_placeholder @@
          let kind = [txt @@ Kind.Dance.to_pretty_string @@ Dance.kind' dance] in
          let%lwt by =
            match%lwt Dance.devisers' dance with
            | [] -> lwt_nil
            | devisers -> lwt [txt " by "; Formatters.Person.names' ~links: true devisers]
          in
          lwt (kind @ by)
      );
    ]
    ~share: (Dance dance)
    ~actions: (
      lwt @@
      [Utils.Button.make_a
        ~classes: ["dropdown-item"]
        ~href: (S.const @@ Endpoints.Page.(href DanceEdit) id)
        ~icon: "pencil-square"
        ~label: "Edit"
        ();
      Utils.Action.delete
        ~onclick: (fun () -> Madge_client.call Endpoints.Api.(route @@ Dance Delete) (Entry.id dance))
        ~model: "dance"
        ();
      ] @ (
        match Dance.scddb_id' dance with
        | None -> []
        | Some scddb_id ->
          [
            a
              ~a: [
                a_class ["dropdown-item"];
                a_href (Uri.to_string @@ SCDDB.dance_uri scddb_id);
              ]
              [
                i ~a: [a_class ["bi"; "bi-box-arrow-up-right"]] [];
                txt " See on SCDDB";
              ]
          ]
      );
    )
    [
      div
        (
          match Dance.two_chords' dance with
          | Some false -> []
          | Some true -> [txt "Two Chords"]
          | None -> [txt "Two Chords: unknown"]
        );
      div
        (
          match Dance.date' dance with
          | None -> []
          | Some date -> [txt "Devised "; txt (PartialDate.to_pretty_string ~at: true date); txt "."]
        );
      div
        [
          h3 [txt "Recommended Tunes"];
          R.div
            (
              S.from' (Tables.placeholder ()) @@
                let%lwt tunes =
                  snd
                  <$> Madge_client.call_exn Endpoints.Api.(route @@ Tune Search) Slice.everything @@
                    Filter.Tune.existsdance' @@ Filter.Dance.is' dance
                in
                lwt
                  [
                    if tunes = [] then
                      txt
                        (
                          "There are no recommended tunes for this dance. " ^
                          "Dancelor is not all-knowing: go check the Strathspey Database! " ^
                          "And if you find something that is not known here, report it to someone."
                        )
                    else
                      Tables.tunes tunes
                  ]
            )
        ];
    ]
