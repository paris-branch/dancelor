open Nes
include Dancelor_common_model.Version

let group = group >=>| Tune.get
let arranger = arranger >=>?| (Credit.get >=>| Lwt.return_some)
let sources = sources >=>| Lwt_list.map_p Source.get

let content _t =
  assert false (* FIXME *)

(* * *)

let get slug =
  Madge_client.(
    call ~endpoint:Endpoint.get @@ fun {a} _ ->
    a Arg.slug slug
  )

let all ?filter ?pagination () =
  Madge_client.(
    call ~endpoint:Endpoint.all @@ fun _ {o} ->
    o Arg.filter filter;
    o Arg.pagination pagination
  )

let search ?filter ?pagination ?threshold string =
  Madge_client.(
    call ~endpoint:Endpoint.search @@ fun {a} {o} ->
    o Arg.filter filter;
    o Arg.pagination pagination;
    o Arg.threshold threshold;
    a Arg.string string
  )

let count ?filter () =
  Madge_client.(
    call ~endpoint:Endpoint.count @@ fun _ {o} -> o Arg.filter filter
  )
