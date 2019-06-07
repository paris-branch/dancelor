open Nes
include Dancelor_common_model.TuneGroup

let author = author >=>?| (Credit.get >=>| Lwt.return_some)

(* * *)

let get = Dancelor_server_database.TuneGroup.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun {a} _ ->
    get (a Arg.slug)
  )
