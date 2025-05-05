open Nes
open Common

let get env slug =
  Lwt.bind_return
    (Model.Set.get slug)
    (Permission.assert_can_get env)

let create env set =
  Permission.assert_can_create env;%lwt
  Database.Set.create set

let update env slug set =
  Lwt.bind (get env slug) (Permission.assert_can_update env);%lwt
  Database.Set.update slug set

let delete env slug =
  Lwt.bind (get env slug) (Permission.assert_can_delete env);%lwt
  Database.Set.delete slug

include Search.Build(struct
  type value = Model.Set.t Entry.t
  type filter = Model.Set.Filter.t

  let get_all env =
    Lwt.map
      (List.filter (Permission.can_get env))
      (Database.Set.get_all ())

  let filter_accepts = Model.Set.Filter.accepts

  let tiebreakers =
    Lwt_list.[increasing (Lwt.return % Model.Set.name) String.Sensible.compare;
    increasing (Lwt.return % Model.Set.name) String.compare_lengths;
    ]
end)

module Pdf = struct
  let render set set_parameters rendering_parameters =
    let contents = [Model.Book.InlineSet (Entry.value set, set_parameters)] in
    let book = Entry.make_dummy @@ Model.Book.make ~title: "" ~contents () in
    let book_parameters = Model.BookParameters.make () in
    Book.Pdf.render book book_parameters rendering_parameters

  let get env set set_parameters rendering_parameters =
    let%lwt set = Model.Set.get set in
    Permission.assert_can_get env set;%lwt
    let%lwt path_pdf = render set set_parameters rendering_parameters in
    Madge_server.respond_file ~content_type: "application/pdf" ~fname: path_pdf
end

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Set.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
  | Pdf -> Pdf.get env
