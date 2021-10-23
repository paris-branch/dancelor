open Nes
module E = Dancelor_common_model.Credit_endpoints
module A = E.Arguments

module Self = struct
  include Dancelor_common_model.Credit

  let persons = persons >=>| Lwt_list.map_p Person.get
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

let get slug =
  Madge_client.(
    call ~endpoint:E.get @@ fun {a} _ ->
    a A.slug slug
  )

let make_and_save ?status ~line ?persons () =
  Madge_client.(
    call ~endpoint:E.make_and_save @@ fun {a} {o} ->
    o A.status status;
    a A.line line;
    o A.persons persons
  )

let search ?pagination ?threshold filter =
  Madge_client.(
    call ~endpoint:E.search @@ fun {a} {o} ->
    o A.pagination pagination;
    o A.threshold threshold;
    a A.filter filter
  )
