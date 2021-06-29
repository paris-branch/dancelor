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

module Filter = struct
  include Filter

  let accepts filter set =
    match filter with

    | Is set' ->
      equal set set'

    | Deviser dfilter ->
      (match%lwt deviser set with
       | None -> Lwt.return_false
       | Some deviser -> Credit.Filter.accepts dfilter deviser)

    | ExistsVersion vfilter ->
      let%lwt versions_and_parameters = versions_and_parameters set in
      Lwt_list.exists_s
        (fun (version, _) ->
           Version.Filter.accepts vfilter version)
        versions_and_parameters
end

let get = Dancelor_server_database.Set.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )

let apply_filter filter all =
  Lwt_list.filter_s (Filter.accepts filter) all

let apply_filter_on_scores filter all =
  Score.list_filter_lwt (Filter.accepts filter) all

let all ?filter ?pagination () =
  Dancelor_server_database.Set.get_all ()
  >>=| Option.unwrap_map_or ~default:Lwt.return apply_filter filter
  >>=| Lwt_list.(sort_multiple [
      increasing name String.Sensible.compare
    ])
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:E.all @@ fun _ {o} ->
    all ?filter:(o A.filter) ?pagination:(o A.pagination) ()
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

let search ?filter ?pagination ?(threshold=0.) string =
  Dancelor_server_database.Set.get_all ()
  >>=| Score.lwt_map_from_list (search string)
  >>=| Option.unwrap_map_or ~default:Lwt.return apply_filter_on_scores filter
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing [
      increasing name String.Sensible.compare
    ])
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:E.search @@ fun {a} {o} ->
    search
      ?filter:    (o A.filter)
      ?pagination:(o A.pagination)
      ?threshold: (o A.threshold)
      (a A.string)
  )

let count () =
  let%lwt l = all () in
  Lwt.return (List.length l)

let () =
  Madge_server.register ~endpoint:E.count @@ fun _ _ -> count ()
