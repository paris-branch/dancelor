open Nes
module E = Dancelor_common_model.Credit_endpoints
module A = E.Arguments

module Self = struct
  include Dancelor_common_model.Credit

  let persons = persons >=>| Lwt_list.map_s Person.get
end
include Self

module Filter = struct
  include Filter

  let accepts filter credit =
    let char_equal = Char.Sensible.equal in
    Formula.interpret filter @@ function

    | Is credit' ->
      equal credit credit' >|=| Formula.interpret_bool

    | Line string ->
      let%lwt line = Self.line credit in
      Lwt.return (String.proximity ~char_equal string line)

    | LineMatches string ->
      let%lwt line = Self.line credit in
      Lwt.return (String.inclusion_proximity ~char_equal ~needle:string line)

    | ExistsPerson pfilter ->
      persons credit
      >>=| Formula.interpret_exists (Person.Filter.accepts pfilter)
end

let get = Dancelor_server_database.Credit.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
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

let search ?pagination ?(threshold=0.) filter =
  Dancelor_server_database.Credit.get_all ()
  >>=| Score.lwt_map_from_list (Filter.accepts filter)
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
      (a A.filter)
  )
