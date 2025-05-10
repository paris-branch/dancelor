open Nes
open Common

include Tables.Set

let get slug = get slug
let get_all () = get_all ()

let get_books_that_contain (slug : ModelBuilder.Core.Set.t Slug.t) : ModelBuilder.Core.Book.t Entry.t list Lwt.t =
  let%lwt all = Tables.Book.get_all () in
  Lwt.return (List.filter (ModelBuilder.Core.Book.contains_set slug) all)

exception UsedInBook of ModelBuilder.Core.Book.t Slug.t
let usedInBook book = UsedInBook book

let delete (set : ModelBuilder.Core.Set.t Slug.t) =
  match%lwt get_books_that_contain set with
  | [] -> delete set
  | book :: _ -> Lwt.fail @@ usedInBook @@ Entry.slug book
