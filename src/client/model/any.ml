include Dancelor_common_model.Any

module Filter = struct
  include Filter

  let accepts filter any =
    Formula.interpret filter @@ function
    | Is any' -> equal any any'
    | TypeIs type_ -> Lwt.return (Type.equal (type_of any) type_)
end

module E = Dancelor_common_model.Any_endpoints
module A = E.Arguments

let search ?filter ?pagination ?threshold string =
  Madge_client.(
    call ~endpoint:E.search @@ fun {a} {o} ->
    o A.filter     filter;
    o A.pagination pagination;
    o A.threshold  threshold;
    a A.string     string
  )
