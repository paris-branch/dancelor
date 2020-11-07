open Nes
include Dancelor_common_model.Person

(* * *)

let get = Dancelor_server_database.Person.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun {a} _ ->
    get (a Arg.slug)
  )

let make_and_save ?status ~name () =
  Dancelor_server_database.Person.save ~slug_hint:name @@ fun slug ->
  Lwt.return (make ?status ~slug ~name ()) (* status should probably go in save *)

let () =
  Madge_server.(
    register ~endpoint:Endpoint.make_and_save @@ fun {a} _ ->
    make_and_save
      ~name:(a Arg.name)
      ()
  )

let search string person =
  let%lwt name = name person in
  String.sensible_inclusion_proximity ~needle:string name
  |> Lwt.return

let search ?pagination ?(threshold=0.) string =
  Dancelor_server_database.Person.get_all ()
  >>=| Score.lwt_map_from_list (search string)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.list_proj_sort_decreasing ~proj:name String.sensible_compare
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:Endpoint.search @@ fun {a} {o} ->
    search
      ?pagination:(o Arg.pagination)
      ?threshold: (o Arg.threshold)
      (a Arg.string)
  )
