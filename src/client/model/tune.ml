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
  Madge.(call ~endpoint:Endpoint.get @@ fun query ->
         add_arg query Arg.slug slug)

let all ?filter ?pagination () =
  Madge.(call ~endpoint:Endpoint.all @@ fun query ->
         add_opt_arg query Arg.filter filter;
         add_opt_arg query Arg.pagination pagination)

let search ?filter ?pagination ?threshold terms =
  Madge.(call ~endpoint:Endpoint.search @@ fun query ->
         add_opt_arg query Arg.filter filter;
         add_opt_arg query Arg.pagination pagination;
         add_opt_arg query Arg.threshold threshold;
         add_arg query     Arg.terms terms)
