open Nes

module Lift
    (Person : module type of PersonSignature)
    (Dance  : module type of  DanceSignature)
= struct
  include TuneCore

  let make
      ?status ~slug ~name ?alternative_names ~kind ?composer ?dances
      ?remark ?scddb_id ?date ~modified_at ~created_at
      ()
    =
    let name = String.remove_duplicates ~char:' ' name in
    let alternative_names = Option.map (List.map (String.remove_duplicates ~char:' ')) alternative_names in
    let composer = Option.map Person.slug composer in
    let dances = Option.map (List.map Dance.slug) dances in
    Lwt.return (make
                  ?status ~slug ~name ?alternative_names ~kind ~composer ?dances
                  ?remark ~scddb_id ~date ~modified_at ~created_at
                  ())

  let composer tune = Option.fold ~none:Lwt.return_none ~some:(Lwt.map Option.some % Person.get) (composer tune)
  let dances tune = Lwt_list.map_p Dance.get (dances tune)

  module Filter = struct
    (* NOTE: [include TuneCore.Filter] shadows the accessors of [TuneCore]. *)
    let tuneCore_composer = composer
    let tuneCore_dances = dances

    include TuneCore.Filter

    let accepts filter tune =
      let char_equal = Char.Sensible.equal in
      Formula.interpret filter @@ function

      | Is tune' ->
        Lwt.return @@ Formula.interpret_bool @@ Slug.equal' (slug tune) tune'

      | Name string ->
        Lwt.return @@ String.proximity ~char_equal string @@ TuneCore.name tune

      | NameMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle:string @@ TuneCore.name tune

      | Composer afilter ->
        Lwt.bind
          (tuneCore_composer tune)
          (Option.fold
             ~none: (Lwt.return Formula.interpret_false)
             ~some: (Person.Filter.accepts afilter))

      | Kind kfilter ->
        Kind.Base.Filter.accepts kfilter @@ TuneCore.kind tune

      | ExistsDance dfilter ->
        let%lwt dances = tuneCore_dances tune in
        let%lwt scores = Lwt_list.map_s (Dance.Filter.accepts dfilter) dances in
        Lwt.return (Formula.interpret_or_l scores)

    let text_formula_converter =
      TextFormulaConverter.(
        make
          [
            raw (Result.ok % nameMatches');
            unary_string ~name:"name"         (name, unName);
            unary_string ~wrap_back:Never ~name:"name-matches" (nameMatches, unNameMatches);
            unary_lift   ~name:"composer"       (composer, unComposer)           ~converter:Person.Filter.text_formula_converter;
            unary_lift   ~name:"by"           (composer, unComposer)           ~converter:Person.Filter.text_formula_converter; (* alias for composer; FIXME: make this clearer *)
            unary_lift   ~name:"kind"         (kind, unKind)               ~converter:Kind.Base.Filter.text_formula_converter;
            unary_lift   ~name:"exists-dance" (existsDance, unExistsDance) ~converter:Dance.Filter.text_formula_converter;
            unary_string ~name:"is"           (is % Slug.unsafe_of_string, Option.map Slug.to_string % unIs);
          ]
      )

    let from_text_formula = TextFormula.to_formula text_formula_converter
    let from_string ?filename input =
      Result.bind (TextFormula.from_string ?filename input) from_text_formula

    let to_text_formula = TextFormula.of_formula text_formula_converter
    let to_string = TextFormula.to_string % to_text_formula

    let is = is % slug
    let is' = Formula.pred % is

    let composerIs = composer % Person.Filter.is'
    let composerIs' = Formula.pred % composerIs

    (* Little trick to convince OCaml that polymorphism is OK. *)
    type op = { op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t }

    let optimise =
      let lift {op} f1 f2 = match (f1, f2) with
        | (Composer f1, Composer f2) -> Option.some @@ composer (op f1 f2)
        | (Kind f1, Kind f2) -> Option.some @@ kind (op f1 f2)
        | (ExistsDance f1, ExistsDance f2) -> Option.some @@ existsDance (op f1 f2)
        | _ -> None
      in
      Formula.optimise
        ~lift_and: (lift {op = Formula.and_})
        ~lift_or: (lift {op = Formula.or_})
        (function
          | (Is _ as p) | (Name _ as p) | (NameMatches _ as p) -> p
          | Composer pfilter -> composer @@ Person.Filter.optimise pfilter
          | Kind kfilter -> kind @@ Kind.Base.Filter.optimise kfilter
          | ExistsDance dfilter -> existsDance @@ Dance.Filter.optimise dfilter)
  end
end
