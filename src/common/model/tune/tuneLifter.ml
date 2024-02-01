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
    let author = Option.map Person.slug author in
    let dances = Option.map (List.map Dance.slug) dances in
    Lwt.return (make
                  ?status ~slug ~name ?alternative_names ~kind ~author ?dances
                  ?remark ~scddb_id ~modified_at ~created_at
                  ())

  let author tune = Option.fold ~none:Lwt.return_none ~some:(Lwt.map Option.some % Person.get) (author tune)
  let dances tune = Lwt_list.map_p Dance.get (dances tune)

  module Filter = struct
    include TuneCore.Filter

    let accepts filter tune =
      let char_equal = Char.Sensible.equal in
      Formula.interpret filter @@ function

      | Is tune' ->
        Lwt.return @@ Formula.interpret_bool @@ equal tune tune'

      | Name string ->
        Lwt.return @@ String.proximity ~char_equal string @@ name tune

      | NameMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle:string @@ name tune

      | Author afilter ->
        (match%lwt author tune with
         | None -> Formula.interpret_false |> Lwt.return
         | Some author -> Person.Filter.accepts afilter author)

      | Kind kfilter ->
        Kind.Base.Filter.accepts kfilter @@ kind tune

      | ExistsDance dfilter ->
        let%lwt dances = dances tune in
        let%lwt scores = Lwt_list.map_s (Dance.Filter.accepts dfilter) dances in
        Lwt.return (Formula.interpret_or_l scores)

    let is tune = Formula.pred (Is tune)
    let name string = Formula.pred (Name string)
    let nameMatches string = Formula.pred (NameMatches string)
    let author cfilter = Formula.pred (Author cfilter)
    let authorIs author_ = author (Person.Filter.is author_)
    let kind kfilter = Formula.pred (Kind kfilter)
    let existsDance dfilter = Formula.pred (ExistsDance dfilter)

    let raw = Result.ok % nameMatches

    let nullary_text_predicates =
      TextFormula.[
        nullary ~name:"reel"       (kind Kind.Base.(Filter.is Reel));       (* alias for kind:reel       FIXNE: make this clearer *)
        nullary ~name:"jig"        (kind Kind.Base.(Filter.is Jig));        (* alias for kind:jig        FIXNE: make this clearer *)
        nullary ~name:"strathspey" (kind Kind.Base.(Filter.is Strathspey)); (* alias for kind:strathspey FIXNE: make this clearer *)
        nullary ~name:"waltz"      (kind Kind.Base.(Filter.is Waltz));      (* alias for kind:waltz      FIXNE: make this clearer *)
      ]

    let unary_text_predicates =
      TextFormula.[
        unary ~name:"name"         (raw_only ~convert:no_convert name);
        unary ~name:"name-matches" (raw_only ~convert:no_convert nameMatches);
        unary ~name:"author"       (author @@@@ Person.Filter.from_text_formula);
        unary ~name:"by"           (author @@@@ Person.Filter.from_text_formula); (* alias for author; FIXME: make this clearer *)
        unary ~name:"kind"         (kind @@@@ Kind.Base.Filter.from_text_formula);
        unary ~name:"exists-dance" (existsDance @@@@ Dance.Filter.from_text_formula);
      ]

    let from_text_formula =
      TextFormula.make_to_formula
        raw
        nullary_text_predicates
        unary_text_predicates

    let from_string ?filename input =
      from_text_formula (TextFormula.from_string ?filename input)
  end
end
