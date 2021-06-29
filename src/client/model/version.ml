open Nes
module E = Dancelor_common_model.Version_endpoints
module A = E.Arguments

include Dancelor_common_model.Version

let tune = tune >=>| Tune.get
let arranger = arranger >=>?| (Credit.get >=>| Lwt.return_some)
let sources = sources >=>| Lwt_list.map_p Source.get

let content _t =
  assert false (* FIXME *)

module Filter = struct
  include Filter

  let accepts filter version =
    match filter with
    | Is version' ->
      let%lwt slug' = slug version' in
      let%lwt slug  = slug version  in
      Lwt.return (Slug.equal slug slug')
    | Tune tfilter ->
      let%lwt tune = tune version in
      Tune.Filter.accepts tfilter tune
    | Key key' ->
      let%lwt key = key version in
      Lwt.return (key = key')
    | Bars bars' ->
      let%lwt bars = bars version in
      Lwt.return (bars = bars')
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

let search ?filter ?pagination ?threshold string =
  Madge_client.(
    call ~endpoint:E.search @@ fun {a} {o} ->
    o A.filter filter;
    o A.pagination pagination;
    o A.threshold threshold;
    a A.string string
  )

let count ?filter () =
  Madge_client.(
    call ~endpoint:E.count @@ fun _ {o} -> o A.filter filter
  )
