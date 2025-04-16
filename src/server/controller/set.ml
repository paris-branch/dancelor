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

  let cache = Cache.create ~lifetime: 600 ()
  let get_all = Database.Set.get_all
  let filter_accepts = Model.Set.Filter.accepts

  let tiebreakers =
    Lwt_list.[increasing (Lwt.return % Model.Set.name) String.Sensible.compare;
    increasing (Lwt.return % Model.Set.name) String.compare_lengths;
    ]
end)

module Pdf = struct
  let render parameters set =
    let book =
      Entry.make_dummy @@
        Model.Book.make ~title: "" ~contents: [InlineSet (Entry.value set, parameters)] ()
    in
    let parameters =
      (* FIXME: the fact that we need to transfer this is just wrong. see
         https://github.com/paris-branch/dancelor/issues/250 *)
      Model.BookParameters.make ?paper_size: (Model.SetParameters.paper_size parameters) ()
    in
    Book.Pdf.render parameters book

  let get env parameters set =
    let%lwt set = Model.Set.get set in
    Permission.assert_can_get env set;%lwt
    let%lwt path_pdf = render parameters set in
    Madge_cohttp_lwt_server.shortcut @@ Cohttp_lwt_unix.Server.respond_file ~fname: path_pdf ()
end

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Set.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search (* FIXME *)
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
  | Pdf -> Pdf.get env
