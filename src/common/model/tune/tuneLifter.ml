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

    let is tune = Is tune
    let name string = Name string
    let nameMatches string = NameMatches string
    let author cfilter = Author cfilter
    let authorIs author_ = author (Person.Filter.is' author_)
    let kind kfilter = Kind kfilter
    let existsDance dfilter = ExistsDance dfilter

    let is' = Formula.pred % is
    let name' = Formula.pred % name
    let nameMatches' = Formula.pred % nameMatches
    let author' = Formula.pred % author
    let authorIs' = Formula.pred % authorIs
    let kind' = Formula.pred % kind
    let existsDance' = Formula.pred % existsDance

    let text_formula_converter =
      TextFormulaConverter.(
        make
          [
            unary_raw ~name:"name"         (Result.ok % name);
            unary_raw ~name:"name-matches" (Result.ok % nameMatches);
            unary     ~name:"author"       (Result.map author % Person.Filter.from_text_formula);
            unary     ~name:"by"           (Result.map author % Person.Filter.from_text_formula); (* alias for author; FIXME: make this clearer *)
            unary     ~name:"kind"         (Result.map kind % Kind.Base.Filter.from_text_formula);
            unary     ~name:"exists-dance" (Result.map existsDance % Dance.Filter.from_text_formula);
          ]
          ~raw: (Result.ok % nameMatches')
      )

    let from_text_formula = TextFormula.to_formula text_formula_converter
    let from_string ?filename input =
      Result.bind (TextFormula.from_string ?filename input) from_text_formula
  end
end
