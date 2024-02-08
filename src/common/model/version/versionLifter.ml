open Nes

module Lift
    (Person : module type of PersonSignature)
    (Tune   : module type of   TuneSignature)
= struct
  include VersionCore

  let make
      ~slug ?status ~tune ~bars ~key ~structure ?arranger ?remark
      ?disambiguation ?broken ~modified_at ~created_at
      ()
    =
    let structure = String.remove_duplicates ~char:' ' structure in
    let disambiguation = Option.map (String.remove_duplicates ~char:' ') disambiguation in
    let tune = Tune.slug tune in
    let arranger = Option.map Person.slug arranger in
    Lwt.return (make
                  ~slug ?status ~tune ~bars ~key ~structure ~arranger ?remark
                  ?disambiguation ?broken ~modified_at ~created_at
                  ())

  let tune version = Tune.get (tune version)
  let arranger tune = Olwt.flip @@ Option.map Person.get tune.arranger

  let set_broken t broken = {t with broken}

  let kind version =
    Fun.flip Lwt.map (tune version) @@ fun tune ->
    (bars version, Tune.kind tune)

  let name version = Lwt.map Tune.name (tune version)

  module Filter = struct
    let versionCore_tune = tune

    include VersionCore.Filter

    let accepts filter version =
      Formula.interpret filter @@ function

      | Is version' ->
        Lwt.return @@ Formula.interpret_bool @@ equal version version'

      | Tune tfilter ->
        let%lwt tune = versionCore_tune version in
        Tune.Filter.accepts tfilter tune

      | Key key' ->
        Lwt.return @@ Formula.interpret_bool (VersionCore.key version = key')

      | Kind kfilter ->
        let%lwt tune = versionCore_tune version in
        Kind.Version.Filter.accepts kfilter (VersionCore.bars version, Tune.kind tune)

      | Broken ->
        Lwt.return @@ Formula.interpret_bool @@ VersionCore.broken version

    let text_formula_converter =
      TextFormulaConverter.(
        merge
          (
            (* Version-specific converter *)
            make
              [
                nullary    ~name:"broken" broken;

                unary_lift ~name:"tune"   (tune, unTune)   ~converter:Tune.Filter.text_formula_converter;

                unary_raw  ~name:"key"    (key, unKey)     ~cast:(Music.key_of_string_opt, Music.key_to_string) ~type_:"key";

                unary_lift ~name:"kind"   (kind, unKind)   ~converter:Kind.Version.Filter.text_formula_converter;
              ]
          )
          (
            (* Tune converter, lifted to versions *)
            map tune' Tune.Filter.text_formula_converter
          )
      )

    let from_text_formula = TextFormula.to_formula text_formula_converter
    let from_string ?filename input =
      Result.bind
        (TextFormula.from_string ?filename input)
        from_text_formula
  end
end
