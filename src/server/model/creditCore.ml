open Nes
include Dancelor_common_model.CreditCore

let persons = persons >=>| Lwt_list.map_s PersonCore.get

module E = Dancelor_common_model.CreditEndpoints
module A = E.Arguments

let get = Dancelor_server_database.Credit.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )
