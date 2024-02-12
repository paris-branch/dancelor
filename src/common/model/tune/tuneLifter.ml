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
    (* NOTE: [include TuneCore.Filter] shadows the accessors of [TuneCore]. *)
    let tuneCore_author = author
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

      | Author afilter ->
        Lwt.bind
          (tuneCore_author tune)
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
            unary_string ~name:"name-matches" (nameMatches, unNameMatches);
            unary_lift   ~name:"author"       (author, unAuthor)           ~converter:Person.Filter.text_formula_converter;
            unary_lift   ~name:"by"           (author, unAuthor)           ~converter:Person.Filter.text_formula_converter; (* alias for author; FIXME: make this clearer *)
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

    let authorIs = author % Person.Filter.is'
    let authorIs' = Formula.pred % authorIs
  end
end
