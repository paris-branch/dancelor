open Nes
module E = Dancelor_common_model.Set_endpoints
module A = E.Arguments

module Self = struct
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
end
include Self

module Filter = struct
  include Filter

  let accepts filter set =
    let char_equal = Char.Sensible.equal in
    Formula.interpret filter @@ function

    | Is set' ->
      equal set set' >|=| Formula.interpret_bool

    | Name string ->
      let%lwt name = Self.name set in
      Lwt.return (String.proximity ~char_equal string name)

    | NameMatches string ->
      let%lwt name = Self.name set in
      Lwt.return (String.inclusion_proximity ~char_equal ~needle:string name)

    | Deviser dfilter ->
      (match%lwt Self.deviser set with
       | None -> Lwt.return Formula.interpret_false
       | Some deviser -> Credit.Filter.accepts dfilter deviser)

    | ExistsVersion vfilter ->
      let%lwt versions_and_parameters = Self.versions_and_parameters set in
      Formula.interpret_exists
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

let search ?pagination ?(threshold=0.) filter =
  Dancelor_server_database.Set.get_all ()
  >>=| Score.lwt_map_from_list (Filter.accepts filter)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing [
      increasing name String.Sensible.compare;
      increasing name String.compare_lengths;
    ])
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:E.search @@ fun {a} {o} ->
    search
      ?pagination:(o A.pagination)
      ?threshold: (o A.threshold)
      (a A.filter)
  )

let count filter =
  let%lwt l = search filter in
  Lwt.return (List.length l)

let () =
  Madge_server.register ~endpoint:E.count @@ fun {a} _ ->
  count (a A.filter)
