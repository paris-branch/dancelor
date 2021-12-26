open Nes
include Dancelor_common_model.SetCore

let deviser = deviser >=>?| (Credit.get >=>| Lwt.return_some)

let versions_and_parameters set =
  let%lwt versions_and_parameters = versions_and_parameters set in
  Lwt_list.map_s
    (fun (slug, parameters) ->
       let%lwt version = Version.get slug in
       Lwt.return (version, parameters))
    versions_and_parameters

let dances = dances >=>| Lwt_list.map_p Dance.get

let warnings _s = assert false (* FIXME *)

module E = Dancelor_common_model.SetEndpoints
module A = E.Arguments

let get = Dancelor_server_database.Set.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )
