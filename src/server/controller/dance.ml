open NesUnix
open Common

module Log = (val Logger.create "controller.dance": Logs.LOG)

let get env id =
  match%lwt Database.Dance.get id with
  | None -> Permission.reject_can_get ()
  | Some dance ->
    Permission.assert_can_get env dance;%lwt
    lwt dance

let create env dance =
  Permission.assert_can_create env;%lwt
  Database.Dance.create dance

let update env id dance =
  Permission.assert_can_update env =<< get env id;%lwt
  Database.Dance.update id dance

include Search.Build(struct
  type value = Model.Dance.t Entry.t
  type filter = Filter.Dance.t

  let get_all env =
    List.filter (Permission.can_get env) <$> Database.Dance.get_all ()

  let filter_accepts = Filter.Dance.accepts

  let tiebreakers =
    Lwt_list.[increasing (lwt % NEString.to_string % Model.Dance.one_name') String.Sensible.compare]
end)

module Pdf = struct
  let render env dance set_parameters rendering_parameters =
    let kind = Model.Dance.kind' dance in
    let name = Model.Dance.one_name' dance in
    let%lwt versions =
      (* All the versions of all the tunes attached to this dance *)
      snd
      <$> Version.search env Slice.everything @@
        Filter.Version.tune' @@
        Filter.Tune.existsdance' @@
        Filter.Dance.is' dance
    in
    let set =
      Model.Set.make
        ~name: (NEString.prepend "Dance: " name)
        ~kind
        ~contents: (List.map (fun v -> (v, Model.VersionParameters.none)) versions)
        ~order: (List.mapi (fun i _ -> Model.SetOrder.Internal (i + 1)) versions)
        ()
    in
    let set_parameters = Model.SetParameters.set_show_order false set_parameters in
    Set.Pdf.render set set_parameters rendering_parameters

  let get env id _slug set_parameters rendering_parameters =
    match%lwt Database.Dance.get id with
    | None -> Permission.reject_can_get ()
    | Some dance ->
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
