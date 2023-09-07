open Nes

module Lift
    (Credit : module type of CreditSignature)
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
    let%lwt tune = Tune.slug tune in
    let%lwt arranger =
      let%olwt arranger = Lwt.return arranger in
      let%lwt arranger = Credit.slug arranger in
      Lwt.return_some arranger
    in
    Lwt.return (make
                  ~slug ?status ~tune ~bars ~key ~structure ~arranger ?remark
                  ?disambiguation ?broken ~modified_at ~created_at
                  ())

  let tune version = Tune.get version.tune
  let bars t = Lwt.return t.bars
  let key t = Lwt.return t.key
  let structure t = Lwt.return t.structure
  let arranger tune = Olwt.flip @@ Option.map Credit.get tune.arranger
  let remark t = Lwt.return t.remark
  let disambiguation t = Lwt.return t.disambiguation
  let broken t = Lwt.return t.broken

  let set_broken t broken = Lwt.return {t with broken}

  let equal version1 version2 =
    let%lwt slug1 = slug version1 in
    let%lwt slug2 = slug version2 in
    Lwt.return (Slug.equal slug1 slug2)

  module Filter = struct
    include VersionCore.Filter

    let accepts filter version =
      Formula.interpret filter @@ function

      | Is version' ->
        equal version version' >|=| Formula.interpret_bool

      | Tune tfilter ->
        let%lwt tune = tune version in
        Tune.Filter.accepts tfilter tune

      | Key key' ->
        let%lwt key = key version in
        Lwt.return (Formula.interpret_bool (key = key'))

      | Kind kfilter ->
        let%lwt tune = tune version in
        let%lwt kind = Tune.kind tune in
        let%lwt bars = bars version in
        Kind.Version.Filter.accepts kfilter (bars, kind)

      | Broken ->
        broken version >|=| Formula.interpret_bool

    let is version = Formula.pred (Is version)
    let tune tfilter = Formula.pred (Tune tfilter)
    let tuneIs tune_ = tune (Tune.Filter.is tune_)
    let key key_ = Formula.pred (Key key_)
    let kind kfilter = Formula.pred (Kind kfilter)
    let broken = Formula.pred Broken

    let raw string =
      match Tune.Filter.raw string with
      | Ok tfilter -> Ok (tune tfilter)
      | Error err -> Error err (* FIXME: syntext *)

    let nullary_text_predicates = [
      "reel",       (kind Kind.(Version.Filter.base Base.(Filter.is Reel)));       (* alias for kind:reel       FIXME: make this clearer *)
      "jig",        (kind Kind.(Version.Filter.base Base.(Filter.is Jig)));        (* alias for kind:jig        FIXME: make this clearer *)
      "strathspey", (kind Kind.(Version.Filter.base Base.(Filter.is Strathspey))); (* alias for kind:strathspey FIXME: make this clearer *)
      "waltz",      (kind Kind.(Version.Filter.base Base.(Filter.is Waltz)));      (* alias for kind:waltz      FIXME: make this clearer *)
      "broken",      broken;
    ]

    let unary_text_predicates =
      TextFormula.[
        "tune",    (tune @@@@ Tune.Filter.from_text_formula);
        "key",     raw_only ~convert:(fun s -> match Music.key_of_string_opt s with Some k -> Ok k | None -> Error "not a valid key") key;
        "kind",    (kind @@@@ Kind.Version.Filter.from_text_formula);
      ]
      @ (List.map
           (fun (name, pred) ->
              (name, fun x ->
                  match pred x with
                  | Ok tfilter -> Ok (tune tfilter)
                  | Error err -> Error err))
           Tune.Filter.unary_text_predicates)

    let from_text_formula =
      TextFormula.make_to_formula raw
        nullary_text_predicates
        unary_text_predicates

    let from_string ?filename input =
      from_text_formula (TextFormula.from_string ?filename input)
  end
end
