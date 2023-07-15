open Nes

module Lift
    (Credit : module type of CreditSignature)
= struct
  include DanceCore

  let make
      ?status ~slug ~name ~kind ?deviser ~two_chords ?scddb_id
      ?disambiguation ~modified_at ~created_at
      ()
    =
    let name = String.remove_duplicates ~char:' ' name in
    let disambiguation = Option.map (String.remove_duplicates ~char:' ') disambiguation in
    let%lwt deviser =
      match deviser with
      | None -> Lwt.return_none
      | Some deviser ->
        let%lwt deviser = Credit.slug deviser in
        Lwt.return_some deviser
    in
    Lwt.return (make
                  ?status ~slug ~name ~kind ~deviser ~two_chords ~scddb_id
                  ?disambiguation ~modified_at ~created_at
                  ())

  let slug d = Lwt.return d.slug
  let status d = Lwt.return d.status
  let name d = Lwt.return d.name
  let kind d = Lwt.return d.kind
  let deviser dance = Lwt.return @@ Option.map Credit.get dance.deviser
  let two_chords d = Lwt.return d.two_chords
  let scddb_id d = Lwt.return d.scddb_id
  let disambiguation d = Lwt.return d.disambiguation

  let equal dance1 dance2 =
    let%lwt slug1 = slug dance1 in
    let%lwt slug2 = slug dance2 in
    Lwt.return (Slug.equal slug1 slug2)

  module Filter = struct
    include DanceCore.Filter

    let is dance = Formula.pred (Is dance)
    let name name = Formula.pred (Name name)
    let nameMatches name = Formula.pred (NameMatches name)
    let kind kfilter = Formula.pred (Kind kfilter)
    let deviser cfilter = Formula.pred (Deviser cfilter)

    let raw string = Ok (nameMatches string)

    let nullary_text_predicates = [
      "reel",       (kind KindFilter.(Dance.base (Base.is Reel)));       (* alias for kind:reel       FIXNE: make this clearer *)
      "jig",        (kind KindFilter.(Dance.base (Base.is Jig)));        (* alias for kind:jig        FIXNE: make this clearer *)
      "strathspey", (kind KindFilter.(Dance.base (Base.is Strathspey))); (* alias for kind:strathspey FIXNE: make this clearer *)
      "waltz",      (kind KindFilter.(Dance.base (Base.is Waltz)));      (* alias for kind:waltz      FIXNE: make this clearer *)
    ]

    let unary_text_predicates =
      TextFormula.[
        "name",         raw_only ~convert:no_convert name;
        "name-matches", raw_only ~convert:no_convert nameMatches;
        "kind",         (kind @@@@ KindFilter.Dance.from_text_formula);
        "deviser",      (deviser @@@@ Credit.Filter.from_text_formula);
        "by",           (deviser @@@@ Credit.Filter.from_text_formula); (* alias for deviser; FIXME: make this clearer *)
      ]

    let from_text_formula =
      TextFormula.make_to_formula raw
        nullary_text_predicates
        unary_text_predicates
  end
end
