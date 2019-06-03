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
    register ~endpoint:Endpoint.get @@ fun {a} _ ->
    get (a Arg.slug)
  )

let get_all = Dancelor_server_database.Set.get_all

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get_all @@ fun _ _ ->
    get_all ()
  )

let make_and_save ~name ?deviser ~kind ?status ?tunes () =
  let%lwt deviser =
    match deviser with
    | None -> Lwt.return_none
    | Some deviser ->
      let%lwt deviser = Credit.slug deviser in
      Lwt.return_some deviser
  in
  let%lwt tunes =
    match tunes with
    | None -> Lwt.return_none
    | Some tunes ->
      let%lwt tunes = Lwt_list.map_s Tune.slug tunes in
      Lwt.return_some tunes
  in
  Dancelor_server_database.Set.save ~slug_hint:name @@ fun slug ->
  unsafe_make ~slug ~name ?deviser ~kind ?status ?tunes ()

let () =
  Madge_server.(
    register ~endpoint:Endpoint.make_and_save @@ fun {a} {o} ->
    make_and_save
      ~name:   (a Arg.name)
      ?deviser:(o Arg.deviser)
      ~kind:   (a Arg.kind)
      ?status: (o Arg.status)
      ?tunes:  (o Arg.tunes)
      ()
  )

let delete s =
  let%lwt slug = slug s in
  Dancelor_server_database.Set.delete slug

let () =
  Madge_server.(
    register ~endpoint:Endpoint.delete @@ fun {a} _ ->
    let%lwt set = get (a Arg.slug) in
    delete set
  )
