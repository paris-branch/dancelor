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
    include VersionCore.Filter

    let accepts filter version =
      Formula.interpret filter @@ function

      | Is version' ->
        Lwt.return @@ Formula.interpret_bool @@ equal version version'

      | Tune tfilter ->
        let%lwt tune = tune version in
        Tune.Filter.accepts tfilter tune

      | Key key' ->
        Lwt.return @@ Formula.interpret_bool (key version = key')

      | Kind kfilter ->
        let%lwt tune = tune version in
        Kind.Version.Filter.accepts kfilter (bars version, Tune.kind tune)

      | Broken ->
        Lwt.return @@ Formula.interpret_bool @@ broken version

    let is version = Is version
    let tune tfilter = Tune tfilter
    let tuneIs tune_ = tune (Tune.Filter.is' tune_)
    let key key_ = Key key_
    let kind kfilter = Kind kfilter
    let broken = Broken

    let is' = Formula.pred % is
    let tune' = Formula.pred % tune
    let tuneIs' = Formula.pred % tuneIs
    let key' = Formula.pred % key
    let kind' = Formula.pred % kind
    let broken' = Formula.pred broken

    let text_formula_converter =
      TextFormulaConverter.(
        merge
          (
            (* Version-specific converter *)
            make
              [
                nullary   ~name:"broken"  broken;
                unary     ~name:"tune"   (Result.map tune % Tune.Filter.from_text_formula);
                unary_raw ~name:"key"    (Result.map key % Option.to_result ~none:"not a valid key" % Music.key_of_string_opt);
                unary     ~name:"kind"   (Result.map kind % Kind.Version.Filter.from_text_formula);
              ]
              ~raw: (Result.map tune' % raw Tune.Filter.text_formula_converter)
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
