include Dancelor_common_model.Set

let deviser s =
  match%lwt deviser s with
  | None -> Lwt.return_none
  | Some deviser ->
    let%lwt deviser = Dancelor_server_database.Credit.get deviser in
    Lwt.return_some deviser

let tunes s =
  let%lwt tunes = tunes s in
  Lwt_list.map_s Dancelor_server_database.Tune.get tunes

let warnings _s = assert false (* FIXME *)

(* * *)

let get = Dancelor_server_database.Set.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun query ->
    get (get_arg query Arg.slug)
  )

let get_all = Dancelor_server_database.Set.get_all

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get_all @@ fun _ ->
    get_all ()
  )

let make_and_save ~name ?deviser ~kind ?status ?tunes () =
  Dancelor_server_database.Set.save ~slug_hint:name @@ fun slug ->
  unsafe_make ~slug ~name ?deviser ~kind ?status ?tunes ()

let () =
  Madge_server.(
    register ~endpoint:Endpoint.make_and_save @@ fun query ->
    make_and_save
      ~name:   (get_arg     query Arg.name)
      ?deviser:(get_opt_arg query Arg.deviser)
      ~kind:   (get_arg     query Arg.kind)
      ?status: (get_opt_arg query Arg.status)
      ?tunes:  (get_opt_arg query Arg.tunes)
      ()
  )

let delete s =
  let%lwt slug = slug s in
  Dancelor_server_database.Set.delete slug

let () =
  Madge_server.(
    register ~endpoint:Endpoint.delete @@ fun query ->
    let%lwt set = get (get_arg query Arg.slug) in
    delete set
  )
