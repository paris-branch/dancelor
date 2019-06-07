open Nes
include Dancelor_common_model.Credit

let persons = persons >=>| Lwt_list.map_p Person.get

(* * *)

let get slug =
  Madge_client.(
    call ~endpoint:Endpoint.get @@ fun {a} _ ->
    a Arg.slug slug
  )

let make_and_save ~line ?persons () =
  Madge_client.(
    call ~endpoint:Endpoint.make_and_save @@ fun {a} {o} ->
    a Arg.line line;
    o Arg.persons persons
  )

let search ?threshold terms =
  Madge_client.(
    call ~endpoint:Endpoint.search @@ fun {a} {o} ->
    o Arg.threshold threshold;
    a Arg.terms terms
  )
