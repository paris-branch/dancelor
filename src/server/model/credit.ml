open Nes
include Dancelor_common_model.Credit

let persons = persons >=>| Lwt_list.map_s Person.get

(* * *)

let get = Dancelor_server_database.Credit.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun {a} _ ->
    get (a Arg.slug)
  )

let make_and_save ~line ?persons () =
  let%lwt persons =
    match persons with
    | None -> Lwt.return_none
    | Some persons ->
      let%lwt persons = Lwt_list.map_s Person.slug persons in
      Lwt.return_some persons
  in
  Dancelor_server_database.Credit.save ~slug_hint:line @@ fun slug ->
  Lwt.return (make ~slug ~line ?persons ())

let () =
  Madge_server.(
    register ~endpoint:Endpoint.make_and_save @@ fun {a} {o} ->
    make_and_save
      ~line:   (a Arg.line)
      ?persons:(o Arg.persons)
      ()
  )

let search string credit =
  let%lwt line = line credit in
  String.sensible_inclusion_proximity ~needle:string line
  |> Lwt.return

let search ?pagination ?(threshold=0.) string =
  Dancelor_server_database.Credit.get_all ()
  >>=| Score.lwt_map_from_list (search string)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.list_proj_sort_decreasing ~proj:line String.sensible_compare
  >>=| Option.unwrap_map_or Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:Endpoint.search @@ fun {a} {o} ->
    search
      ?pagination:(o Arg.pagination)
      ?threshold: (o Arg.threshold)
      (a Arg.string)
  )
