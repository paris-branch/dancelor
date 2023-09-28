open Lwt
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_html
open Dancelor_client_model
module Formatters = Dancelor_client_formatters

module Html = Dom_html

let js = Js.string

type td = { td : 'node_list. ?colspan:int -> ('node_list, node list) kind -> 'node_list -> node }
(** See {!make_clickable_row}. *)

(** An oddly specific helper. Takes an [href], a [prefix], and [children] and
    returns an {!Lwt} promise holding one table row {!tr} whose cells are the
    ones from [prefix] catenated with the ones from [children]. Each cell is
    wrapped in an [a ~href]. To achieve this, [prefix] and [children] are in
    fact functions that expects as arguments something compatible with {!td}. *)
let make_clickable_row ~href ~prefix children =
  let td ?colspan kind children =
    td ?colspan const [
      a ~classes:["fill"] ~href:(const href) kind children
    ]
  in
  (* let td_const ?colspan _ children = *)
  (*   td ?colspan const [ *)
  (*     a ~classes:["fill"] ~href:(const href) kind children *)
  (*   ] *)
  (* in *)
  Lwt.return (tr ~classes:["clickable"] const (prefix { td } @ children { td }))

let make_credit_result ~prefix credit =
  let%lwt slug = Credit.slug credit in
  let href = PageRouter.(path (Credit slug)) in
  make_clickable_row ~href ~prefix @@ fun { td } ->
  [
    td ~colspan:3 const [text lwt (Credit.line credit)]
  ]

let make_dance_result ~prefix dance =
  let%lwt slug = Dance.slug dance in
  let href = PageRouter.(path (Dance slug)) in
  make_clickable_row ~href ~prefix @@ fun { td } ->
  [
    td const [text lwt (Dance.name dance)];
    td const [text lwt (Dance.kind dance >|= Kind.Dance.to_string)];
    td lwt (Dance.deviser dance >>= Formatters.Credit.line);
  ]

let make_book_result ~prefix book =
  let%lwt slug = Book.slug book in
  let href = PageRouter.(path (Book slug)) in
  make_clickable_row ~href ~prefix @@ fun { td } ->
  [
    td lwt ~colspan:3 (Formatters.Book.title_and_subtitle book)
  ]

let make_set_result ~prefix set =
  let%lwt slug = Set.slug set in
  let href = PageRouter.(path (Set slug)) in
  make_clickable_row ~href ~prefix @@ fun { td } ->
  [
    td const [text lwt (Set.name set)];
    td const [text lwt (Set.kind set >|= Kind.Dance.to_string)];
    td lwt (Set.deviser set >>= Formatters.Credit.line);
  ]

let make_tune_result ~prefix tune =
  let%lwt slug = Tune.slug tune in
  let href = PageRouter.(path (Tune slug)) in
  make_clickable_row ~href ~prefix @@ fun { td } ->
  [
    td const [text lwt (Tune.name tune)];
    td const [text lwt (Tune.kind tune >|= Kind.Base.to_pretty_string ~capitalised:true)];
    td lwt (Tune.author tune >>= Formatters.Credit.line);
  ]

let make_version_result ~prefix version =
  let%lwt slug = Version.slug version in
  let href = PageRouter.(path (Version slug)) in
  let%lwt tune = Version.tune version in
  make_clickable_row ~href ~prefix @@ fun { td } ->
  [
    td lwt (Formatters.Version.name_and_disambiguation ~link:false version);
    td const [text lwt (
        let%lwt bars = Version.bars version in
        let%lwt kind = Tune.kind tune in
        let%lwt structure = Version.structure version in
        Lwt.return (Kind.Version.to_string (bars, kind) ^ " (" ^ structure ^ ")")
      )];
    td lwt (Formatters.Version.author_and_arranger version);
  ]

let make_result score =
  let any = Score.value score in
  let prefix { td } = [
    td const [text const (Score.score_to_string score)];
    td const [text const (any |> Any.type_of |> Any.Type.to_string)];
  ]
  in
  match any with
  | Credit credit   -> make_credit_result  ~prefix credit
  | Dance dance     -> make_dance_result   ~prefix dance
  | Book book       -> make_book_result    ~prefix book
  | Set set         -> make_set_result     ~prefix set
  | Tune tune       -> make_tune_result    ~prefix tune
  | Version version -> make_version_result ~prefix version
