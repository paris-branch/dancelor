open Nes
module E = Dancelor_common_model.Tune_endpoints
module A = E.Arguments

include Dancelor_common_model.Tune

let author = author >=>?| (Credit.get >=>| Lwt.return_some)

let dances = dances >=>| Lwt_list.map_p Dance.get

module Filter = struct
  include Filter

  let accepts filter tune =
    match filter with
    | Is tune' ->
      let%lwt slug' = slug tune' in
      let%lwt slug  = slug tune  in
      Lwt.return (Slug.equal slug slug')
    | Author afilter ->
      (match%lwt author tune with
       | None -> Lwt.return_true
       | Some author -> Credit.Filter.accepts afilter author)
    | AuthorIsDefined ->
      let%lwt author = author tune in
      Lwt.return (author <> None)
    | Kind kind' ->
      let%lwt kind = kind tune in
      Lwt.return (kind = kind')
end

let get slug =
  Madge_client.(
    call ~endpoint:E.get @@ fun {a} _ ->
    a A.slug slug
  )

let search ?pagination ?threshold string =
  Madge_client.(
    call ~endpoint:E.search @@ fun {a} {o} ->
    o A.pagination pagination;
    o A.threshold threshold;
    a A.string string
  )
