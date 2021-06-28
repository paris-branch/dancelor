open Nes
module E = Dancelor_common_model.Credit_endpoints
module A = E.Arguments

include Dancelor_common_model.Credit

let persons = persons >=>| Lwt_list.map_p Person.get

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
    call ~endpoint:E.get @@ fun {a} _ ->
    a A.slug slug
  )

let all ?filter ?pagination () =
  Madge_client.(
    call ~endpoint:E.all @@ fun _ {o} ->
    o A.filter filter;
    o A.pagination pagination
  )

let make_and_save ?status ~line ?persons () =
  Madge_client.(
    call ~endpoint:E.make_and_save @@ fun {a} {o} ->
    o A.status status;
    a A.line line;
    o A.persons persons
  )

let search ?pagination ?threshold string =
  Madge_client.(
    call ~endpoint:E.search @@ fun {a} {o} ->
    o A.pagination pagination;
    o A.threshold threshold;
    a A.string string
  )
