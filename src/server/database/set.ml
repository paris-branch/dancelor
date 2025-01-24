open Nes
module Model = Dancelor_common_model
module Database = Dancelor_common_database

include Tables.Set

let get slug = get slug
let get_all () = get_all ()

let get_books_that_contain (slug : Model.Core.Set.t Slug.t) : Model.Core.Book.t Database.Entry.t list Lwt.t =
  let%lwt all = Tables.Book.get_all () in
  Lwt.return (List.filter (Model.Core.Book.contains_set slug) all)

exception UsedInBook of Model.Core.Book.t Slug.t
let usedInBook book = UsedInBook book

let delete (set : Model.Core.Set.t Slug.t) =
  match%lwt get_books_that_contain set with
  | [] -> delete set
  | book :: _ -> Lwt.fail @@ usedInBook @@ Entry.slug book
