open Nes

module Lift
    (Person : module type of PersonSignature)
= struct
  include DanceCore

  let make
      ?status ~slug ~name ~kind ?deviser ~two_chords ?scddb_id
      ?disambiguation ~modified_at ~created_at
      ()
    =
    let name = String.remove_duplicates ~char:' ' name in
    let disambiguation = Option.map (String.remove_duplicates ~char:' ') disambiguation in
    let deviser = Option.map Person.slug deviser in
    Lwt.return (make
                  ?status ~slug ~name ~kind ~deviser ~two_chords ~scddb_id
                  ?disambiguation ~modified_at ~created_at
                  ())

  let deviser dance = Option.fold ~none:Lwt.return_none ~some:(Lwt.map Option.some % Person.get) (deviser dance)

  let equal dance1 dance2 = Slug.equal (slug dance1) (slug dance2)

  module Filter = struct
    include DanceCore.Filter

    let accepts filter dance =
      let char_equal = Char.Sensible.equal in
      Formula.interpret filter @@ function

      | Is dance' ->
        Lwt.return @@ Formula.interpret_bool @@ equal dance dance'

      | Name string ->
        Lwt.return @@ String.proximity ~char_equal string @@ name dance

      | NameMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle:string @@ name dance

      | Kind kfilter ->
        Kind.Dance.Filter.accepts kfilter @@ kind dance

      | Deviser cfilter ->
        (match%lwt deviser dance with
         | None -> Lwt.return Formula.interpret_false
         | Some deviser -> Person.Filter.accepts cfilter deviser)

    let is dance = Formula.pred (Is dance)
    let name name = Formula.pred (Name name)
    let nameMatches name = Formula.pred (NameMatches name)
    let kind kfilter = Formula.pred (Kind kfilter)
    let deviser cfilter = Formula.pred (Deviser cfilter)

    let raw string = Ok (nameMatches string)

    let nullary_text_predicates =
      TextFormula.[
        nullary ~name: "reel"       (kind Kind.(Dance.Filter.base Base.(Filter.is Reel)));       (* alias for kind:reel       FIXME: make this clearer *)
        nullary ~name: "jig"        (kind Kind.(Dance.Filter.base Base.(Filter.is Jig)));        (* alias for kind:jig        FIXME: make this clearer *)
        nullary ~name: "strathspey" (kind Kind.(Dance.Filter.base Base.(Filter.is Strathspey))); (* alias for kind:strathspey FIXME: make this clearer *)
        nullary ~name: "waltz"      (kind Kind.(Dance.Filter.base Base.(Filter.is Waltz)));      (* alias for kind:waltz      FIXME: make this clearer *)
      ]

    let unary_text_predicates =
      TextFormula.[
        unary ~name: "name"         (raw_only ~convert:no_convert name);
        unary ~name: "name-matches" (raw_only ~convert:no_convert nameMatches);
        unary ~name: "kind"         (kind @@@@ Kind.Dance.Filter.from_text_formula);
        unary ~name: "deviser"      (deviser @@@@ Person.Filter.from_text_formula);
        unary ~name: "by"           (deviser @@@@ Person.Filter.from_text_formula); (* alias for deviser; FIXME: make this clearer *)
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
