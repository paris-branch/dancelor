open Dancelor_client_model
module Html = Dancelor_client_html
module PageRouter = Dancelor_common_pageRouter

let make ?(a=[]) ?context content any =
  (* FIXME: [Any] should re-export the constructors (ppx_import?) *)
  let open Dancelor_common_model.AnyCore in
  let href = match any with
    | Version version -> PageRouter.path_version ?context (Version.slug version)
    | Set set -> PageRouter.path_set ?context (Set.slug set)
    | Person person -> PageRouter.path_person ?context (Person.slug person)
    | Dance dance -> PageRouter.path_dance ?context (Dance.slug dance)
    | Book book -> PageRouter.path_book ?context (Book.slug book)
    | Tune tune -> PageRouter.path_tune ?context (Tune.slug tune)
  in
  Html.a ~a:(Html.a_href href :: a) content
