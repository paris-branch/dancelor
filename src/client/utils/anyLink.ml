open Dancelor_client_model
open Dancelor_client_html
module PageRouter = Dancelor_common_pageRouter

let make ?context content any =
  (* FIXME: [Any] should re-export the constructors (ppx_import?) *)
  let open Dancelor_common_model.AnyCore in
  let href = match any with
    | Version version -> PageRouter.path_version ?context (Version.slug version)
    | Set set -> PageRouter.path_set (Set.slug set)
    | Person person -> PageRouter.path_person (Person.slug person)
    | Dance dance -> PageRouter.path_dance (Dance.slug dance)
    | Book book -> PageRouter.path_book (Book.slug book)
    | Tune tune -> PageRouter.path_tune (Tune.slug tune)
  in
  a ~a:[a_href href] content
