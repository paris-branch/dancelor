open Nes

module Lift
    (Person : module type of PersonSignature)
    (Dance  : module type of  DanceSignature)
= struct
  include TuneCore

  let make
      ?status ~slug ~name ?alternative_names ~kind ?author ?dances
      ?remark ?scddb_id ~modified_at ~created_at
      ()
    =
    let name = String.remove_duplicates ~char:' ' name in
    let alternative_names = Option.map (List.map (String.remove_duplicates ~char:' ')) alternative_names in
    let%lwt author =
      let%olwt author = Lwt.return author in
      let%lwt author_slug = Person.slug author in
      Lwt.return_some author_slug
    in
    let%lwt dances =
      let%olwt dances = Lwt.return dances in
      let%lwt dances =
        Lwt_list.map_s
          (fun dance ->
             let%lwt dance = Dance.slug dance in
             Lwt.return dance)
          dances
      in
      Lwt.return_some dances
    in
    Lwt.return (make
                  ?status ~slug ~name ?alternative_names ~kind ~author ?dances
                  ?remark ~scddb_id ~modified_at ~created_at
                  ())

  let name tune = Lwt.return tune.name
  let alternative_names tune = Lwt.return tune.alternative_names
  let kind tune = Lwt.return tune.kind
  let author tune = Olwt.flip @@ Option.map Person.get tune.author
  let dances tune = Lwt_list.map_p Dance.get tune.dances
  let remark tune = Lwt.return tune.remark
  let scddb_id tune = Lwt.return tune.scddb_id

  let compare =
    Slug.compare_slugs_or
      ~fallback:(fun tune1 tune2 ->
          Lwt.return (Stdlib.compare tune1 tune2))
      slug

  let equal = equal_from_compare compare

  module Filter = struct
    include TuneCore.Filter

    let accepts filter tune =
      let char_equal = Char.Sensible.equal in
      Formula.interpret filter @@ function

      | Is tune' ->
        equal tune tune' >|=| Formula.interpret_bool

      | Name string ->
        let%lwt name = name tune in
        Lwt.return (String.proximity ~char_equal string name)

      | NameMatches string ->
        let%lwt name = name tune in
        Lwt.return (String.inclusion_proximity ~char_equal ~needle:string name)

      | Author afilter ->
        (match%lwt author tune with
         | None -> Formula.interpret_false |> Lwt.return
         | Some author -> Person.Filter.accepts afilter author)

      | Kind kfilter ->
        let%lwt kind = kind tune in
        Kind.Base.Filter.accepts kfilter kind

      | ExistsDance dfilter ->
        let%lwt dances = dances tune in
        let%lwt scores = Lwt_list.map_s (Dance.Filter.accepts dfilter) dances in
        Lwt.return (Formula.interpet_or_l scores)

    let is tune = Formula.pred (Is tune)
    let name string = Formula.pred (Name string)
    let nameMatches string = Formula.pred (NameMatches string)
    let author cfilter = Formula.pred (Author cfilter)
    let authorIs author_ = author (Person.Filter.is author_)
    let kind kfilter = Formula.pred (Kind kfilter)
    let existsDance dfilter = Formula.pred (ExistsDance dfilter)

    let raw string = Ok (nameMatches string)

    let nullary_text_predicates = [
      "reel",       (kind Kind.Base.(Filter.is Reel));       (* alias for kind:reel       FIXNE: make this clearer *)
      "jig",        (kind Kind.Base.(Filter.is Jig));        (* alias for kind:jig        FIXNE: make this clearer *)
      "strathspey", (kind Kind.Base.(Filter.is Strathspey)); (* alias for kind:strathspey FIXNE: make this clearer *)
      "waltz",      (kind Kind.Base.(Filter.is Waltz));      (* alias for kind:waltz      FIXNE: make this clearer *)
    ]

    let unary_text_predicates =
      TextFormula.[
        "name",         raw_only ~convert:no_convert name;
        "name-matches", raw_only ~convert:no_convert nameMatches;
        "author",       (author @@@@ Person.Filter.from_text_formula);
        "by",           (author @@@@ Person.Filter.from_text_formula); (* alias for author; FIXME: make this clearer *)
        "kind",         (kind @@@@ Kind.Base.Filter.from_text_formula);
        "exists-dance", (existsDance @@@@ Dance.Filter.from_text_formula);
      ]

    let from_text_formula =
      TextFormula.make_to_formula raw
        nullary_text_predicates
        unary_text_predicates

    let from_string ?filename input =
      from_text_formula (TextFormula.from_string ?filename input)
  end
end
