open Nes
module E = Dancelor_common_model.Tune_endpoints
module A = E.Arguments

module Self = struct
  include Dancelor_common_model.Tune

  let author = author >=>?| (Credit.get >=>| Lwt.return_some)
  let dances = dances >=>| Lwt_list.map_p Dance.get
end
include Self

module Filter = struct
  include Filter

  let accepts filter tune =
    let char_equal = Char.Sensible.equal in
    Formula.interpret filter @@ function

    | Is tune' ->
      equal tune tune' >|=| Formula.interpret_bool

    | Name string ->
      let%lwt name = Self.name tune in
      Lwt.return (String.proximity ~char_equal string name)

    | NameMatches string ->
      let%lwt name = Self.name tune in
      Lwt.return (String.inclusion_proximity ~char_equal ~needle:string name)

    | Author afilter ->
      (match%lwt Self.author tune with
       | None -> Formula.interpret_false |> Lwt.return
       | Some author -> Credit.Filter.accepts afilter author)

    | Kind kind' ->
      let%lwt kind = Self.kind tune in
      Lwt.return (Formula.interpret_bool (kind = kind'))
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
