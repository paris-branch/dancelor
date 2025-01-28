open Nes
open Dancelor_common

include Tables.Set

let get slug = get slug
let get_all () = get_all ()

let get_books_that_contain (slug : ModelBuilder.Set.t Slug.t) : ModelBuilder.Book.t Entry.t list Lwt.t =
  let%lwt all = Tables.Book.get_all () in
  Lwt.return (List.filter (ModelBuilder.Book.contains_set slug) all)

exception UsedInBook of ModelBuilder.Book.t Slug.t
let usedInBook book = UsedInBook book

let delete (set : ModelBuilder.Set.t Slug.t) =
  match%lwt get_books_that_contain set with
  | [] -> delete set
  | book :: _ -> Lwt.fail @@ usedInBook @@ Entry.slug book
