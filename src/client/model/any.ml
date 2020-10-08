include Dancelor_common_model.Any

let search ?pagination ?threshold ?except string =
  Madge_client.(
    call ~endpoint:Endpoint.search @@ fun {a} {o} ->
    o Arg.pagination pagination;
    o Arg.threshold threshold;
    o Arg.type_ except;
    a Arg.string string
  )
