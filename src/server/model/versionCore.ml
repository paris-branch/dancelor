open Nes
include Dancelor_common_model.VersionCore

let tune = tune >=>| Tune.get
let arranger = arranger >=>?| (Credit.get >=>| Lwt.return_some)

let content = Dancelor_server_database.Version.read_content

module E = Dancelor_common_model.VersionEndpoints
module A = E.Arguments

let get = Dancelor_server_database.Version.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )
