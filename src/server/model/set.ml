open Nes
module E = Dancelor_common_model.Set_endpoints
module A = E.Arguments

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
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )

let all ?pagination () =
  Dancelor_server_database.Set.get_all ()
  >>=| Lwt_list.(sort_multiple [
      increasing name String.Sensible.compare
    ])
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:E.all @@ fun _ {o} ->
    all ?pagination:(o A.pagination) ()
  )

let make_and_save ?status ~name ?deviser ~kind ?versions_and_parameters ?dances () =
  Dancelor_server_database.Set.save ~slug_hint:name @@ fun slug ->
  make ?status ~slug ~name ?deviser ~kind ?versions_and_parameters ?dances ()

let () =
  Madge_server.(
    register ~endpoint:E.make_and_save @@ fun {a} {o} ->
    make_and_save
      ~name:   (a A.name)
      ?deviser:(o A.deviser)
      ~kind:   (a A.kind)
      ?status: (o A.status)
      ?versions_and_parameters:(o A.versions_and_parameters)
      ?dances:(o A.dances)
      ()
  )

let delete s =
  let%lwt slug = slug s in
  Dancelor_server_database.Set.delete slug

let () =
  Madge_server.(
    register ~endpoint:E.delete @@ fun {a} _ ->
    let%lwt set = get (a A.slug) in
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
  >>=| Score.(list_proj_sort_decreasing [
      increasing name String.Sensible.compare
    ])
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:E.search @@ fun {a} {o} ->
    search
      ?pagination:(o A.pagination)
      ?threshold: (o A.threshold)
      (a A.string)
  )

let count () =
  let%lwt l = all () in
  Lwt.return (List.length l)

let () =
  Madge_server.register ~endpoint:E.count @@ fun _ _ -> count ()
