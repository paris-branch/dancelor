open Nes
open Dancelor_common_model
include CreditCore

let persons = persons >=>| Lwt_list.map_p Person.get

module E = CreditEndpoints
module A = E.Arguments

let get slug =
  Madge_client.(
    call ~endpoint:E.get @@ fun {a} _ ->
    a A.slug slug
  )
