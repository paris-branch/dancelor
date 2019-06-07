open Nes
include Dancelor_common_model.Source

(* * *)

let get = Dancelor_server_database.Source.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun {a} _ ->
    get (a Arg.slug)
  )

let make_and_save ~name () =
  Dancelor_server_database.Source.save ~slug_hint:name @@ fun slug ->
  Lwt.return (make ~slug ~name)

let () =
  Madge_server.(
    register ~endpoint:Endpoint.make_and_save @@ fun {a} _ ->
    make_and_save
      ~name:(a Arg.name)
      ()
  )

let search string source =
  let%lwt name = name source in
  String.inclusion_proximity ~needle:string name
  |> Lwt.return

let search ?pagination ?(threshold=0.) string =
  Dancelor_server_database.Source.get_all ()
  >>=| Score.lwt_map_from_list (search string)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.list_proj_sort_decreasing ~proj:name String.sensible_compare
  >>=| Option.unwrap_map_or Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:Endpoint.search @@ fun {a} {o} ->
    search
      ?pagination:(o Arg.pagination)
      ?threshold: (o Arg.threshold)
      (a Arg.string)
  )
