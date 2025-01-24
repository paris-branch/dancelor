open NesUnix
module Model = Dancelor_server_model
module Database = Dancelor_server_database
module Log = (val Dancelor_server_logs.create "controller.dance": Logs.LOG)

module Pdf = struct
  let render parameters dance =
    let kind = Model.Dance.kind dance in
    let name = Model.Dance.name dance in
    let%lwt versions =
      (* All the versions of all the tunes attached to this dance *)
      Model.Version.search' @@
      Model.Version.Filter.tune' @@
      Model.Tune.Filter.existsDance' @@
      Model.Dance.Filter.is' dance
    in
    let parameters = Model.SetParameters.set_show_order false parameters in
    let set =
      Database.Entry.make_dummy @@
      Model.Set.make
        ~name: ("Dance: " ^ name)
        ~kind
        ~contents: (List.map (fun v -> (v, Model.VersionParameters.none)) versions)
        ~order: (List.mapi (fun i _ -> Model.SetOrder.Internal (i + 1)) versions)
        ()
    in
    Set.Pdf.render parameters set

  let get parameters dance_slug =
    let%lwt dance = Model.Dance.get dance_slug in
    let%lwt path_pdf = render parameters dance in
    Madge_cohttp_lwt_server.shortcut @@ Cohttp_lwt_unix.Server.respond_file ~fname: path_pdf ()
end

let dispatch : type a r. (a, r Lwt.t, r) Dancelor_common_model.Endpoints.Dance.t -> a = function
  | Get -> Model.Dance.get
  | Search -> Model.Dance.search
  | Create -> Model.Dance.create
  | Update -> Model.Dance.update
  | Pdf -> Pdf.get
