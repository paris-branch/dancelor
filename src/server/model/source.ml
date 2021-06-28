open Nes
module E = Dancelor_common_model.Source_endpoints
module A = E.Arguments

include Dancelor_common_model.Source

let get = Dancelor_server_database.Source.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )

let make_and_save ?status ~name () =
  Dancelor_server_database.Source.save ~slug_hint:name @@ fun slug ->
  Lwt.return (make ?status ~slug ~name ()) (* status should probably go in save *)

let () =
  Madge_server.(
    register ~endpoint:E.make_and_save @@ fun {a} _ ->
    make_and_save
      ~name:(a A.name)
      ()
  )

let search string source =
  let%lwt name = name source in
  String.inclusion_proximity ~char_equal:Char.Sensible.equal ~needle:string name
  |> Lwt.return

let search ?pagination ?(threshold=0.) string =
  Dancelor_server_database.Source.get_all ()
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
