include Dancelor_common_model.Tune

let group t =
  let%lwt slug = group t in
  TuneGroup.get slug

let arranger t =
  match%lwt arranger t with
  | None -> Lwt.return_none
  | Some slug ->
    let%lwt c = Credit.get slug in
    Lwt.return_some c

let dances t =
  let%lwt dances = dances t in
  Lwt_list.map_p Dance.get dances

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

let search ?filter ?pagination ?threshold terms =
  Madge_client.(
    call ~endpoint:Endpoint.search @@ fun {a} {o} ->
    o Arg.filter filter;
    o Arg.pagination pagination;
    o Arg.threshold threshold;
    a Arg.terms terms
  )
