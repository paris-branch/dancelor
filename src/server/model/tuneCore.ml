open Nes
include Dancelor_common_model.TuneCore

let author = author >=>?| (Credit.get >=>| Lwt.return_some)
let dances = dances >=>| Lwt_list.map_p Dance.get

module E = Dancelor_common_model.TuneEndpoints
module A = E.Arguments

let get = Dancelor_server_database.Tune.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )
