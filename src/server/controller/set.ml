open Nes
open Common

let get = Model.Set.get

let create = Database.Set.create
let update = Database.Set.update
let save = Database.Set.save

let delete = Database.Set.delete

include ModelBuilder.Search.Build(struct
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

  let get parameters set =
    let%lwt set = Model.Set.get set in
    let%lwt path_pdf = render parameters set in
    Madge_cohttp_lwt_server.shortcut @@ Cohttp_lwt_unix.Server.respond_file ~fname: path_pdf ()
end

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Set.t -> a = fun _env endpoint ->
  match endpoint with
  | Get -> get
  | Delete -> delete
  | Search -> search
  | Create -> create
  | Update -> update
  | Pdf -> Pdf.get
