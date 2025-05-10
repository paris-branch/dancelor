open NesUnix
open Common

module Log = (val Logger.create "controller.dance": Logs.LOG)

let get env slug =
  Lwt.bind_return
    (Model.Dance.get slug)
    (Permission.assert_can_get env)

let create env dance =
  Permission.assert_can_create env;%lwt
  Database.Dance.create dance

let update env slug dance =
  Lwt.bind (get env slug) (Permission.assert_can_update env);%lwt
  Database.Dance.update slug dance

include Search.Build(struct
  type value = Model.Dance.t Entry.t
  type filter = Model.Dance.Filter.t

  let get_all env =
    Lwt.map
      (List.filter (Permission.can_get env))
      (Database.Dance.get_all ())

  let filter_accepts = Model.Dance.Filter.accepts

  let tiebreakers =
    Lwt_list.[increasing (Lwt.return % Model.Dance.name') String.Sensible.compare]
end)

module Pdf = struct
  let render env dance set_parameters rendering_parameters =
    let kind = Model.Dance.kind' dance in
    let name = Model.Dance.name' dance in
    let%lwt versions =
      (* All the versions of all the tunes attached to this dance *)
      Lwt.map snd @@
      Version.search env Slice.everything @@
      Model.Version.Filter.tune' @@
      Model.Tune.Filter.existsDance' @@
      Model.Dance.Filter.is' dance
    in
    let set =
      Model.Set.make
        ~name: ("Dance: " ^ name)
        ~kind
        ~contents: (List.map (fun v -> (v, Model.VersionParameters.none)) versions)
        ~order: (List.mapi (fun i _ -> Model.SetOrder.Internal (i + 1)) versions)
        ()
    in
    let set_parameters = Model.SetParameters.set_show_order false set_parameters in
    Set.Pdf.render set set_parameters rendering_parameters

  let get env dance_slug set_parameters rendering_parameters =
    let%lwt dance = Model.Dance.get dance_slug in
    Permission.assert_can_get env dance;%lwt
    let%lwt path_pdf = render env dance set_parameters rendering_parameters in
    Madge_server.respond_file ~content_type: "application/pdf" ~fname: path_pdf
end

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Dance.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Pdf -> Pdf.get env
