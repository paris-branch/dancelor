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
    (* NOTE: [include DanceCore.Filter] shadows the accessors of [DanceCore]. *)
    let danceCore_deviser = deviser

    include DanceCore.Filter

    let accepts filter dance =
      let char_equal = Char.Sensible.equal in
      Formula.interpret filter @@ function

      | Is dance' ->
        Lwt.return @@ Formula.interpret_bool @@ equal dance dance'

      | Name string ->
        Lwt.return @@ String.proximity ~char_equal string @@ DanceCore.name dance

      | NameMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle:string @@ DanceCore.name dance

      | Kind kfilter ->
        Kind.Dance.Filter.accepts kfilter @@ DanceCore.kind dance

      | Deviser cfilter ->
        (match%lwt danceCore_deviser dance with
         | None -> Lwt.return Formula.interpret_false
         | Some deviser -> Person.Filter.accepts cfilter deviser)

    let text_formula_converter =
      TextFormulaConverter.(
        make
          [
            unary_string ~name: "name"         name;
            unary_string ~name: "name-matches" nameMatches;
            unary        ~name: "kind"         (Result.map kind % Kind.Dance.Filter.from_text_formula);
            unary        ~name: "deviser"      (Result.map deviser % Person.Filter.from_text_formula);
            unary        ~name: "by"           (Result.map deviser % Person.Filter.from_text_formula); (* alias for deviser; FIXME: make this clearer *)
          ]
          ~raw: (Result.ok % nameMatches')
      )

    let from_text_formula = TextFormula.to_formula text_formula_converter
    let from_string ?filename input =
      Result.bind (TextFormula.from_string ?filename input) from_text_formula
  end
end
