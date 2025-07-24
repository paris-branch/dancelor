open Nes
open Common

include Tables.Set

let get id = get id
let get_all () = get_all ()

let get_books_that_contain (id : ModelBuilder.Core.Set.t Entry.Id.t) : ModelBuilder.Core.Book.t Entry.t list Lwt.t =
  let%lwt all = Tables.Book.get_all () in
  lwt (List.filter (ModelBuilder.Core.Book.contains_set id) all)

exception UsedInBook of ModelBuilder.Core.Book.t Entry.Id.t
let usedInBook book = UsedInBook book

let delete (set : ModelBuilder.Core.Set.t Entry.Id.t) =
  match%lwt get_books_that_contain set with
  | [] -> delete set
  | book :: _ -> Lwt.fail @@ usedInBook @@ Entry.id book
