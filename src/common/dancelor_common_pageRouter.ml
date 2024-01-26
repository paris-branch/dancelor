(** {1 Client Router} *)

open Nes
open Dancelor_common_model

(** Context in which a page might exist. TODO: I wonder whether it'd be possible
    to simply use [page] here, having pages carry a “parent” [page option]. *)
type context =
  | InSearch of string

let inSearch query = InSearch query

(** Existing pages in Dancelor's client. *)
type page =
  | Index
  | BookAll
  | BookCompose
  | BookEdit of BookCore.t Slug.t
  | Book of {slug: BookCore.t Slug.t; context: context option}
  | PersonAdd
  | Person of {slug: PersonCore.t Slug.t; context: context option}
  | Dance of {slug: DanceCore.t Slug.t; context: context option}
  | Search of string option
  | SetAll
  | SetCompose
  | Set of {slug: SetCore.t Slug.t; context: context option}
  | Tune of {slug: TuneCore.t Slug.t; context: context option}
  | VersionAdd
  | VersionAll
  | VersionBroken
  | Version of {slug : VersionCore.t Slug.t; context: context option}

(* FIXME: It would be so much nicer if [Search] could carry an actual
   [AnyCore.Filter.predicate Formula.t]. That however requires moving a lot of
   code from [*Lifter] to [*Core] for all models (basically everything but the
   [accepts] function, I would say), so, for now, we keep it as a string. *)

let book ?context slug = Book {slug; context}
let bookEdit slug = BookEdit slug
let person ?context slug = Person {slug; context}
let dance ?context slug = Dance {slug; context}
let search q = Search q
let set ?context slug = Set {slug; context}
let tune ?context slug = Tune {slug; context}
let version ?context slug = Version {slug; context}

let unBookEdit = function BookEdit slug -> Some slug | _ -> None

open Madge_router
module MQ = Madge_query

let context_of_query = Option.map inSearch % MQ.get_string "in-search"
let context_to_query = function
  | None -> MQ.empty
  | Some (InSearch query) -> MQ.singleton "in-search" (`String query)

let routes =
  (* NOTE: It is important that [with_slug] instances come after more specific
     ones. For instance, the [with_slug] corresponding to "/book/{slug}" should
     come after "/book/all", so as to avoid matching "all" as a slug. *)
  [
    direct    `GET "/"                Index ;
    direct    `GET "/book/all"        BookAll ;
    direct    `GET "/book/compose"    BookCompose ;
    with_slug `GET "/book/edit"      (bookEdit, unBookEdit) ;

    with_slug_and_query `GET "/book"
      (fun slug query -> book slug ?context:(context_of_query query))
      (function Book {slug; context} -> Some (slug, context_to_query context) | _ -> None) ;

    direct    `GET "/person/add"      PersonAdd ;

    with_slug_and_query `GET "/person"
      (fun slug query -> person slug ?context:(context_of_query query))
      (function Person {slug; context} -> Some (slug, context_to_query context) | _ -> None) ;

    with_slug_and_query `GET "/dance"
      (fun slug query -> dance slug ?context:(context_of_query query))
      (function Dance {slug; context} -> Some (slug, context_to_query context) | _ -> None) ;

    with_query `GET "/search"
      (fun query -> search @@ MQ.get_string "q" query)
      (function
        | Search None -> Some MQ.empty
        | Search (Some q) -> Option.some @@ MQ.singleton "q" (`String q)
        | _ -> None) ;

    direct    `GET "/set/all"         SetAll ;
    direct    `GET "/set/compose"     SetCompose ;

    with_slug_and_query `GET "/set"
      (fun slug query -> set slug ?context:(context_of_query query))
      (function Set {slug; context} -> Some (slug, context_to_query context) | _ -> None) ;

    with_slug_and_query `GET "/tune"
      (fun slug query -> tune slug ?context:(context_of_query query))
      (function Tune {slug; context} -> Some (slug, context_to_query context) | _ -> None) ;

    direct    `GET "/version/add"     VersionAdd ;
    direct    `GET "/version/all"     VersionAll ;
    direct    `GET "/version/broken"  VersionBroken ;

    with_slug_and_query `GET "/version"
      (fun slug query -> version slug ?context:(context_of_query query))
      (function Version {slug; context} -> Some (slug, context_to_query context) | _ -> None) ;
  ]

let path page =
  Madge_router.resource_to_request page routes
  |> Madge_router.request_to_uri
  |> Uri.to_string

(* Shorter versions for when we want immediately a path, which is the case the
   majority of the time. *)
let path_book = path % book
let path_bookEdit = path % bookEdit
let path_person = path % person
let path_dance = path % dance
let path_search = path % search
let path_set = path % set
let path_tune ?context slug = path @@ tune ?context slug
let path_version ?context slug = path @@ version ?context slug
