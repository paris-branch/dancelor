open Nes
module E = Dancelor_common_model.Version_endpoints
module A = E.Arguments

module Self = struct
  include Dancelor_common_model.Version

  let tune = tune >=>| Tune.get
  let arranger = arranger >=>?| (Credit.get >=>| Lwt.return_some)
  let sources = sources >=>| Lwt_list.map_p Source.get

  let content _t =
    assert false (* FIXME *)
end
include Self

module Filter = struct
  include Filter

  let accepts filter version =
    Formula.interpret filter @@ function

    | Is version' ->
      equal version version' >|=| Formula.interpret_bool

    | Tune tfilter ->
      let%lwt tune = Self.tune version in
      Tune.Filter.accepts tfilter tune

    | Key key' ->
      let%lwt key = Self.key version in
      Lwt.return (Formula.interpret_bool (key = key'))

    | BarsEq bars' ->
      let%lwt bars = Self.bars version in
      Lwt.return (Formula.interpret_bool (bars = bars'))
    | BarsNe bars' ->
      let%lwt bars = Self.bars version in
      Lwt.return (Formula.interpret_bool (bars <> bars'))
    | BarsGt bars' ->
      let%lwt bars = Self.bars version in
      Lwt.return (Formula.interpret_bool (bars > bars'))
    | BarsGe bars' ->
      let%lwt bars = Self.bars version in
      Lwt.return (Formula.interpret_bool (bars >= bars'))
    | BarsLt bars' ->
      let%lwt bars = Self.bars version in
      Lwt.return (Formula.interpret_bool (bars < bars'))
    | BarsLe bars' ->
      let%lwt bars = Self.bars version in
      Lwt.return (Formula.interpret_bool (bars <= bars'))
end

let get slug =
  Madge_client.(
    call ~endpoint:E.get @@ fun {a} _ ->
    a A.slug slug
  )

let search ?pagination ?threshold filter =
  Madge_client.(
    call ~endpoint:E.search @@ fun {a} {o} ->
    o A.pagination pagination;
    o A.threshold threshold;
    a A.filter filter;
  )

let count filter =
  Madge_client.(
    call ~endpoint:E.count @@ fun {a} _ ->
    a A.filter filter
  )
