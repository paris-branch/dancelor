open Nes
include Dancelor_common_model.Credit

let persons = persons >=>| Lwt_list.map_p Person.get

(* * *)

module Filter = struct
  include Filter

  let accepts filter credit =
    match filter with
    | ExistsPerson pfilter ->
      persons credit
      >>=| Lwt_list.exists_s (Person.Filter.accepts pfilter)
    | ForallPersons pfilter ->
      persons credit
      >>=| Lwt_list.for_all_s (Person.Filter.accepts pfilter)
end

let get slug =
  Madge_client.(
    call ~endpoint:Endpoint.get @@ fun {a} _ ->
    a Arg.slug slug
  )

let all ?filter ?pagination () =
  Madge_client.(
    call ~endpoint:Endpoint.all @@ fun _ {o} ->
    o Arg.filter filter;
    o Arg.pagination pagination
  )

let make_and_save ?status ~line ?persons () =
  Madge_client.(
    call ~endpoint:Endpoint.make_and_save @@ fun {a} {o} ->
    o Arg.status status;
    a Arg.line line;
    o Arg.persons persons
  )

let search ?pagination ?threshold string =
  Madge_client.(
    call ~endpoint:Endpoint.search @@ fun {a} {o} ->
    o Arg.pagination pagination;
    o Arg.threshold threshold;
    a Arg.string string
  )
