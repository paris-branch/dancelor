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

    let is version = Formula.pred (Is version)
    let tune tfilter = Formula.pred (Tune tfilter)
    let tuneIs tune_ = tune (Tune.Filter.is tune_)
    let key key_ = Formula.pred (Key key_)
    let kind kfilter = Formula.pred (Kind kfilter)
    let broken = Formula.pred Broken

    let raw = Result.map tune % Tune.Filter.raw

    let nullary_text_predicates =
      TextFormula.[
        nullary ~name:"reel"       (kind Kind.(Version.Filter.base Base.(Filter.is Reel)));       (* alias for kind:reel       FIXME: make this clearer *)
        nullary ~name:"jig"        (kind Kind.(Version.Filter.base Base.(Filter.is Jig)));        (* alias for kind:jig        FIXME: make this clearer *)
        nullary ~name:"strathspey" (kind Kind.(Version.Filter.base Base.(Filter.is Strathspey))); (* alias for kind:strathspey FIXME: make this clearer *)
        nullary ~name:"waltz"      (kind Kind.(Version.Filter.base Base.(Filter.is Waltz)));      (* alias for kind:waltz      FIXME: make this clearer *)
        nullary ~name:"broken"      broken;
      ]

    let unary_text_predicates =
      TextFormula.[
        unary ~name:"tune"    (tune @@@@ Tune.Filter.from_text_formula);
        unary ~name:"key"     (raw_only ~convert:(Option.to_result ~none:"not a valid key" % Music.key_of_string_opt) key);
        unary ~name:"kind"    (kind @@@@ Kind.Version.Filter.from_text_formula);
      ]
      @ List.map (TextFormula.map_unary ((%) (Result.map tune))) Tune.Filter.unary_text_predicates

    let from_text_formula =
      TextFormula.make_to_formula raw
        nullary_text_predicates
        unary_text_predicates

    let from_string ?filename input =
      from_text_formula (TextFormula.from_string ?filename input)
  end
end
