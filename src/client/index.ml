open Js_of_ocaml
open Dancelor_common
open Dancelor_client_elements
open Dancelor_client_model

module Html = Dom_html

let js = Js.string

type t =
{
  page : Page.t;
  document : Html.document Js.t;
  content : Html.divElement Js.t;
  search : SearchBar.t;
}

let make_version_search_result page score =
  let version = Score.value score in
  let score = score.Score.score in
  let%lwt slug = Version.slug version in
  let%lwt bars = Version.bars version in
  let%lwt tune = Version.tune version in
  let%lwt kind = Tune.kind tune in
  let href = Lwt.return (Router.path_of_controller (Router.Version slug) |> snd) in
  let row = Table.Row.create
    ~href
    ~cells:[
      Table.Cell.text ~text:(Lwt.return (string_of_int (int_of_float (score *. 100.)))) page;
      Table.Cell.text ~text:(
        let%lwt name = Tune.name tune in
        let%lwt disambiguation =
          match%lwt Version.disambiguation version with
          | "" -> Lwt.return ""
          | disambiguation -> Lwt.return (" (" ^ disambiguation ^ ")")
        in
        Lwt.return (name ^ disambiguation)) page;
      Table.Cell.text ~text:(Lwt.return (string_of_int bars)) page;
      Table.Cell.text ~text:(Lwt.return (Kind.base_to_string kind)) page;
      Table.Cell.text ~text:(Version.structure version) page]
    page
  in
  Lwt.return row

let make_set_search_result page score =
  let set = Score.value score in
  let score = score.Score.score in
  let%lwt slug = Set.slug set in
  let href =
    Helpers.build_path ~route:(Router.Set slug) ()
    |> Lwt.return
  in
  let score = string_of_int (int_of_float (score *. 100.)) in
  let cells =
    let open Lwt in [
    Table.Cell.text ~text:(Lwt.return score) page;
    Table.Cell.text ~text:(Set.name set) page;
    Table.Cell.text ~text:(Set.deviser set >>= Formatters.Credit.line) page;
    Table.Cell.text ~span:2 ~text:(Set.kind set >|= Kind.dance_to_string) page]
  in
  let row = Table.Row.create ~href ~cells page in
  Lwt.return row

let make_credit_search_result page score =
  let deviser = Score.value score in
  let score = score.Score.score in
  let%lwt slug = Credit.slug deviser in
  let href =
    Helpers.build_path ~route:(Router.Credit slug) ()
    |> Lwt.return
  in
  let row = Table.Row.create
    ~href
    ~cells:[
      Table.Cell.text ~text:(Lwt.return (string_of_int (int_of_float (score *. 100.)))) page;
      Table.Cell.text ~span:4 ~text:(Credit.line deviser) page]
    page
  in
  Lwt.return row

let make_person_search_result page score =
  let person = Score.value score in
  let score = score.Score.score in
  let%lwt slug = Person.slug person in
  let href =
    Helpers.build_path ~route:(Router.Person slug) ()
    |> Lwt.return
  in
  let score = string_of_int (int_of_float (score *. 100.)) in
  let row = Table.Row.create
    ~href
    ~cells:[
      Table.Cell.text ~text:(Lwt.return (score)) page;
      Table.Cell.text ~span:4 ~text:(Person.name person) page]
    page
  in
  Lwt.return row

let create page =
  let document = Html.window##.document in
  let content = Html.createDiv document in
  let search_versions =
    SearchBar.Section.create
      ~search:(fun input ->
        Version.search ~threshold:0.2 ~pagination:Pagination.{start = 0; end_ = 4} input)
      ~make_result:(fun score -> make_version_search_result page score)
      ~header:(Table.Row.create
        ~cells:[Table.Cell.header_text ~text:(Lwt.return "Versions") ~span:5 page]
        page)
      page
  in
  let search_sets =
    SearchBar.Section.create
      ~search:(fun input ->
        Set.search ~threshold:0.2 ~pagination:Pagination.{start = 0; end_ = 3} input)
      ~make_result:(fun score -> make_set_search_result page score)
      ~header:(Table.Row.create
        ~cells:[Table.Cell.header_text ~text:(Lwt.return "Sets") ~span:5 page]
        page)
      page
  in
  let search_credits =
    SearchBar.Section.create
      ~search:(fun input ->
        Credit.search ~threshold:0.2 ~pagination:Pagination.{start = 0; end_ = 3} input)
      ~make_result:(fun score -> make_credit_search_result page score)
      ~header:(Table.Row.create
        ~cells:[Table.Cell.header_text ~text:(Lwt.return "Credits") ~span:5 page]
        page)
      page
  in
  let search_persons =
    SearchBar.Section.create
      ~search:(fun input ->
        Person.search ~threshold:0.2 ~pagination:Pagination.{start = 0; end_ = 3} input)
      ~make_result:(fun score -> make_person_search_result page score)
      ~header:(Table.Row.create
        ~cells:[Table.Cell.header_text ~text:(Lwt.return "Persons") ~span:5 page]
        page)
      page
  in
  let search =
    SearchBar.create
      ~placeholder:"Search for anything (it's magic!)"
      ~sections:[search_versions; search_sets; search_credits; search_persons]
      ~hide_sections:true
      page
  in
  Dom.appendChild content (SearchBar.root search);
  {page; document; content; search}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  SearchBar.focus t.search
