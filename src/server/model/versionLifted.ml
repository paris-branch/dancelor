open Dancelor_common_model

include VersionLifter.Lift(Credit)(Tune)

let content = Dancelor_server_database.Version.read_content

module E = Dancelor_common_model.VersionEndpoints
module A = E.Arguments

let get = Dancelor_server_database.Version.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )
