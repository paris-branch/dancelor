open Nes
include Dancelor_common_model.Set

let deviser = deviser >=>?| (Credit.get >=>| Lwt.return_some)

let versions_and_parameters set =
  let%lwt versions_and_parameters = versions_and_parameters set in
  Lwt_list.map_s
    (fun (slug, parameters) ->
       let%lwt version = Version.get slug in
       Lwt.return (version, parameters))
    versions_and_parameters

let dances = dances >=>| Lwt_list.map_p Dance.get

let warnings _s = assert false (* FIXME *)

(* * *)

let get = Dancelor_server_database.Set.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun {a} _ ->
    get (a Arg.slug)
  )

let all ?pagination () =
  Dancelor_server_database.Set.get_all ()
  >>=| Lwt_list.proj_sort_s ~proj:name String.Sensible.compare
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:Endpoint.all @@ fun _ {o} ->
    all ?pagination:(o Arg.pagination) ()
  )

let make_and_save ?status ~name ?deviser ~kind ?versions_and_parameters ?dances () =
  Dancelor_server_database.Set.save ~slug_hint:name @@ fun slug ->
  make ?status ~slug ~name ?deviser ~kind ?versions_and_parameters ?dances ()

let () =
  Madge_server.(
    register ~endpoint:Endpoint.make_and_save @@ fun {a} {o} ->
    make_and_save
      ~name:   (a Arg.name)
      ?deviser:(o Arg.deviser)
      ~kind:   (a Arg.kind)
      ?status: (o Arg.status)
      ?versions_and_parameters:(o Arg.versions_and_parameters)
      ?dances:(o Arg.dances)
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

let search string person =
  let%lwt name = name person in
  String.inclusion_proximity ~char_equal:Char.Sensible.equal ~needle:string name
  |> Lwt.return

let search ?pagination ?(threshold=0.) string =
  Dancelor_server_database.Set.get_all ()
  >>=| Score.lwt_map_from_list (search string)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.list_proj_sort_decreasing ~proj:name String.Sensible.compare
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:Endpoint.search @@ fun {a} {o} ->
    search
      ?pagination:(o Arg.pagination)
      ?threshold: (o Arg.threshold)
      (a Arg.string)
  )

let count () =
  let%lwt l = all () in
  Lwt.return (List.length l)

let () =
  Madge_server.register ~endpoint:Endpoint.count @@ fun _ _ -> count ()
