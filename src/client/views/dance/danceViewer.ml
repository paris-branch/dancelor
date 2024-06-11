open Nes
open Dancelor_common
open Dancelor_client_model
module Formatters = Dancelor_client_formatters
module Components = Dancelor_client_components
module Page = Dancelor_client_page
open Dancelor_client_html

let create ?context slug =
  let dance_lwt = Dance.get slug in
  let title = S.from' "" (Lwt.map Dance.name dance_lwt) in
  Page.make ~title:(Page.sub_title "Dance" title) @@
  div [
    Components.ContextLinks.make_and_render
      ?context
      ~this_page: (PageRouter.path_dance slug)
      (Lwt.map Any.dance dance_lwt);

    h2 ~a:[a_class ["title"]] [R.txt title];
    L.h3 ~a:[a_class ["title"]] (
      let kind = [L.txt @@ Lwt.map (Kind.Dance.to_pretty_string % Dance.kind) dance_lwt] in
      let%lwt by =
        match%lwt dance_lwt >>=| Dance.devisers with
        | [] -> Lwt.return_nil
        | devisers -> Lwt.return (txt " by " :: Formatters.Person.names ~link:true devisers)
      in
      Lwt.return (kind @ by)
    );
    L.div (
      Fun.flip Lwt.map dance_lwt @@ fun dance ->
      match Dance.two_chords dance with
      | Some false -> []
      | Some true -> [h3 ~a:[a_class ["title"]] [txt "Two Chords"]]
      | None -> [h3 ~a:[a_class ["title"]] [txt "Two Chords: unknown"]]
    );

    L.div (
      match%lwt Lwt.map Dance.date dance_lwt with
      | None -> Lwt.return_nil
      | Some date ->
        Lwt.return [txt "Devised "; txt (PartialDate.to_pretty_string ~at:true date); txt "."]
    );
    L.div (
      match%lwt Lwt.map Dance.scddb_id dance_lwt with
      | None -> Lwt.return_nil
      | Some scddb_id ->
        let href = Uri.to_string @@ SCDDB.dance_uri scddb_id in
        Lwt.return [
          txt "See on ";
          a ~a:[a_href href; a_target "blank"] [
            txt "the Strathspey Database"
          ];
          txt ".";
        ]
    );

    div ~a:[a_class ["section"]] [
      h3 [txt "Recommended Tunes"];

      L.div (
        let tunes_lwt =
          let%lwt dance = dance_lwt in
          Tune.search' @@ Tune.Filter.existsDance' (Dance.Filter.is' dance)
        in
        let%lwt tunes = tunes_lwt in

        Lwt.return [
          if tunes = [] then
            txt ("There are no recommended tunes for this dance. "
                 ^ "Dancelor is not all-knowing: go check the Strathspey Database! "
                 ^ "And if you find something that is not known here, report it to someone.")
          else
            Dancelor_client_tables.tunes tunes
        ]
      )
    ];
  ]
