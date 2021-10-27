open Nes
module Model = Dancelor_common_model

include Tables.Set

let get slug = get slug
let get_all () = get_all ()

let get_books_that_contain (slug : Model.SetCore.t Slug.t) : Model.BookCore.t list Lwt.t =
  let%lwt all = Tables.Book.get_all () in
  Lwt.return (List.filter (Model.BookCore.contains_set slug) all)

exception UsedInBook of Model.BookCore.t Slug.t

let delete (set : Model.SetCore.t Slug.t) =
  let%lwt all = get_books_that_contain set in
  match all with
  | [] ->
    delete set
  | book :: _ ->
    let%lwt slug = Model.BookCore.slug book in
    Lwt.fail (UsedInBook slug)
