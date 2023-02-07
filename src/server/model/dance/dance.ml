open Nes
include DanceLifted

module E = Dancelor_common_model.DanceEndpoints
module A = E.Arguments

let make_and_save
    ?status
    ~name
    ~kind
    ?deviser
    ~two_chords
    ?scddb_id
    ()
  =
  Dancelor_server_database.Dance.save ~slug_hint: name
  @@ fun slug ->
    make ?status ~slug ~name ~kind ?deviser ~two_chords ?scddb_id ()

let () =
  Madge_server.(register ~endpoint: E.make_and_save
  @@ fun { a } { o } ->
    make_and_save
      ?status: (o A.status)
      ~name: (a A.name)
      ~kind: (a A.kind)
      ?deviser: (o A.deviser)
      ~two_chords: (a A.two_chords)
      ?scddb_id: (o A.scddb_id)
      ())

let search ?pagination ?(threshold = Float.min_float) filter =
  Dancelor_server_database.Dance.get_all ()
  >>=| Score.lwt_map_from_list (DanceFilter.accepts filter)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing
    [
      increasing name String.Sensible.compare;
    ])
  >>=| Option.unwrap_map_or ~default: Lwt.return Pagination.apply pagination

let () =
  Madge_server.(register ~endpoint: E.search
  @@ fun { a } { o } ->
    search
      ?pagination: (o A.pagination)
      ?threshold: (o A.threshold)
      (a A.filter))
