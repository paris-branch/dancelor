open NesUnix
module Model = Dancelor_server_model
module Log = (val Dancelor_server_logs.create "controller.dance": Logs.LOG)

module Pdf = struct
  let render ?parameters dance =
    let kind = Model.Dance.kind dance in
    let slug = Slug.unsafe_coerce @@ Model.Dance.slug dance in
    let name = Model.Dance.name dance in
    let%lwt versions =
      (* All the versions of all the tunes attached to this dance *)
      Model.Version.search' @@
      Model.Version.Filter.tune' @@
      Model.Tune.Filter.existsDance' @@
      Model.Dance.Filter.is' dance
    in
    let%lwt set_parameters = Model.SetParameters.make ~show_order: false () in
    let parameters =
      Option.fold
        ~none: set_parameters
        ~some: (Model.SetParameters.compose set_parameters)
        parameters
    in
    let%lwt set =
      Model.Set.make
        ~slug
        ~name: ("Dance: " ^ name)
        ~kind
        ~contents: (List.map (fun v -> (v, Model.VersionParameters.none)) versions)
        ~order: (List.mapi (fun i _ -> Model.SetOrder.Internal (i + 1)) versions)
        ~modified_at: (Datetime.now ())
        ~created_at: (Datetime.now ())
        ()
    in
    Set.Pdf.render set ~parameters

  let get dance_slug parameters =
    let%lwt dance = Model.Dance.get dance_slug in
    let%lwt path_pdf = render ?parameters dance in
    Madge_server_new.shortcut @@ Cohttp_lwt_unix.Server.respond_file ~fname: path_pdf ()
end

let dispatch : type a r. (a, r Lwt.t, r) Dancelor_common_model.DanceEndpoints.t -> a = function
  | Get -> Model.Dance.get
  | Search -> (fun slice threshold filter -> Model.Dance.search ?slice ?threshold filter)
  | Save ->
    (fun status name kind devisers two_chords scddb_id disambiguation date modified_at created_at ->
       Model.Dance.save ?status ~name ~kind ?devisers ?two_chords ?scddb_id ?disambiguation ?date ~modified_at ~created_at ()
    )
  | Pdf -> (fun parameters dance -> Pdf.get dance parameters)
