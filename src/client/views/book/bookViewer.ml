open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_model
module Formatters = Dancelor_client_formatters
module Components = Dancelor_client_components

let js = Js.string

type t =
  {
    page : Dancelor_client_elements.Page.t;
    content : Dom_html.divElement Js.t;
  }

let display_warnings warnings =
  let open Dancelor_client_html in
  let display_times n =
    if n = 1 then []
    else [txt " ("; txt (Int.to_english_string_times n); txt ")"]
  in
  let rec display_sets = function
    | [] -> []
    | (None, n) :: tl ->
      ([txt "standalone"] @ display_times n)
      :: display_sets tl
    | (Some set, n) :: tl ->
      ([txt "in “";
        span (Formatters.Set.name set);
        txt "”"] @ display_times n)
      :: display_sets tl
  in
  let rec format_set_list = function
    (* If the warning DuplicateVersion has been logged, the list of sets,
       and hence of html nodes generated by [display_sets] above is never empty *)
    | [] -> assert false
    | [hd] -> hd @ [txt "."]
    | hd::tl -> hd @ [txt ", "] @ format_set_list tl
  in
  let display_warning warning =
    match warning with
    | Book.Empty ->
      li [txt "This book does not contain any set"]
    | Book.DuplicateSet set ->
      li [
        txt "Set “";
        span (Formatters.Set.name set);
        txt "” appears several times in this book."
      ]
    | Book.DuplicateVersion (tune, sets_opt) ->
      li (
        txt "Tune “"
        :: span (Formatters.Tune.name tune)
        :: txt "” appears several times: "
        :: (display_sets sets_opt |> format_set_list)
      )
    | Book.SetDanceMismatch (set, dance) ->
      li [
        txt "Set “";
        span (Formatters.Set.name set);
        txt "” does not have the same kind as its associated dance “";
        span (Formatters.Dance.name dance);
        txt "”."
      ]
  in
  List.map display_warning warnings

let table_contents contents =
  let open Dancelor_client_html in
  tablex
    ~a:[a_class ["separated-table"]]
    ~thead:(
      thead [
        tr [
          th [txt "Type"];
          th [txt "Name"];
          th [txt "Kind"];
        ]
      ]
    )
    [
      L.tbody (
        let%lwt contents = contents in
        List.map
          (function
            | Book.Set (set, parameters) ->
              (
                let href = PageRouter.path_set @@ Set.slug set in
                Dancelor_client_tables.clickable_row ~href [
                  Lwt.return [txt "Set"];
                  (Formatters.Set.name_tunes_and_dance ~link:false set parameters);
                  Lwt.return [txt @@ Kind.Dance.to_string @@ Set.kind set]
                ]
              )

            | InlineSet (set, parameters) ->
              (
                tr [
                  td [txt "Set (inline)"];
                  L.td (Formatters.Set.name_tunes_and_dance ~link:false set parameters);
                  td [txt @@ Kind.Dance.to_string @@ Set.kind set];
                ]
              )

            | Version (version, parameters) ->
              (
                let href = PageRouter.path_version @@ Version.slug version in
                Dancelor_client_tables.clickable_row ~href [
                  Lwt.return [txt "Tune"];
                  (Formatters.Version.name_and_dance ~link:false version parameters);
                  Lwt.return [L.txt (
                      let%lwt tune = Version.tune version in
                      Lwt.return (Kind.Version.to_string (Version.bars version, Tune.kind tune))
                    )];
                ]
              )
          )
          contents
        |> Lwt.return
      )
    ]

let create ?context slug page =
  let document = Dancelor_client_elements.Page.document page in
  let content = Dom_html.createDiv document in
  let book_lwt = Book.get slug in

  Lwt.on_success book_lwt (fun book ->
      document##.title := js (Book.title book ^ " | Book | Dancelor");
    );

  let open Dancelor_client_html in

  (
    Dom.appendChild content @@ To_dom.of_div @@ div [
      h2 ~a:[a_class ["title"]] [L.txt @@ Lwt.map Book.title book_lwt];
      h3 ~a:[a_class ["title"]] [L.txt @@ Lwt.map Book.subtitle book_lwt];
      L.div (
        match%lwt Lwt.map Book.scddb_id book_lwt with
        | None -> Lwt.return_nil
        | Some scddb_id ->
          let href = SCDDB.list_uri scddb_id in
          Lwt.return [
            h3 ~a:[a_class ["title"]] [
              a ~a:[a_href href; a_target "blank"] [
                txt "Link to the Strathspey Database"
              ]
            ]
          ]
      );

      Components.ContextLinks.make_and_render
        ?context
        ~search: Explorer.search
        (Lwt.map Any.book book_lwt);

      L.div (
        match%lwt book_lwt >>=| Book.warnings with
        | [] -> Lwt.return []
        | warnings -> Lwt.return [div ~a:[a_class ["warning"]] [ul (display_warnings warnings)]]
      );

      p [
        L.txt (
          match%lwt Lwt.map Book.date book_lwt with
          | None -> Lwt.return ""
          | Some date -> Lwt.return (spf "Date: %s" (NesPartialDate.to_pretty_string date))
        )
      ];

      div ~a:[a_class ["buttons"]] [
        a
          ~a:[
            a_class ["button"];
            a_onclick (fun _ -> Lwt.async (fun () -> Lwt.map ignore (BookDownloadDialog.create_and_open slug)); false);
          ]
          [
            i ~a:[a_class ["fas"; "fa-file-pdf"]] [];
            txt " PDF";
          ];

        a
          ~a:[
            a_class ["button"];
            a_href PageRouter.(path (BookEdit slug))
          ]
          [
            i ~a:[a_class ["fas"; "fa-edit"]] [];
            txt " Edit"
          ]
      ];

      div ~a:[a_class ["section"]] [
        h3 [txt "Contents"];

        table_contents (Lwt.bind book_lwt Book.contents)
      ];
    ]);

  {page; content}

let contents t =
  t.content

let init t =
  ignore t

let refresh t =
  ignore t
