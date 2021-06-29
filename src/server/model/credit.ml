open Nes
module E = Dancelor_common_model.Credit_endpoints
module A = E.Arguments

include Dancelor_common_model.Credit

let persons = persons >=>| Lwt_list.map_s Person.get

(* * *)

module Filter = struct
  include Filter

  let accepts filter credit =
    match filter with
    | Is credit' ->
      let%lwt slug' = slug credit' in
      let%lwt slug  = slug credit  in
      Lwt.return (Slug.equal slug slug')
    | ExistsPerson pfilter ->
      persons credit
      >>=| Lwt_list.exists_s (Person.Filter.accepts pfilter)
    | ForallPersons pfilter ->
      persons credit
      >>=| Lwt_list.for_all_s (Person.Filter.accepts pfilter)
end

let get = Dancelor_server_database.Credit.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )

let apply_filter filter credits =
  Lwt_list.filter_s (Filter.accepts filter) credits

let all ?filter ?pagination () =
  Dancelor_server_database.Credit.get_all ()
  >>=| Option.unwrap_map_or ~default:Lwt.return apply_filter filter
  >>=| Lwt_list.(sort_multiple [
      increasing line String.Sensible.compare
    ])
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:E.all @@ fun _ {o} ->
    all ?filter:(o A.filter) ?pagination:(o A.pagination) ()
  )

let make_and_save ?status ~line ?persons () =
  let%lwt persons =
    match persons with
    | None -> Lwt.return_none
    | Some persons ->
      let%lwt persons = Lwt_list.map_s Person.slug persons in
      Lwt.return_some persons
  in
  Dancelor_server_database.Credit.save ~slug_hint:line @@ fun slug ->
  Lwt.return (make ?status ~slug ~line ?persons ()) (* FIXME: status should probably go in save *)

let () =
  Madge_server.(
    register ~endpoint:E.make_and_save @@ fun {a} {o} ->
    make_and_save
      ~line:   (a A.line)
      ?persons:(o A.persons)
      ()
  )

let search string credit =
  let%lwt line = line credit in
  String.inclusion_proximity ~char_equal:Char.Sensible.equal ~needle:string line
  |> Lwt.return

let search ?pagination ?(threshold=0.) string =
  Dancelor_server_database.Credit.get_all ()
  >>=| Score.lwt_map_from_list (search string)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing [
      increasing line String.Sensible.compare
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
