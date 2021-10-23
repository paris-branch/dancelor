open Nes

module Self = struct
  let _key = "dance"

  type t =
    { slug : t Slug.t ;
      status : Status.t [@default Status.bot] ;
      name : string ;
      kind : Kind.dance ;
      deviser : Credit.t Slug.t option [@default None] }
  [@@deriving yojson]

  let slug d = Lwt.return d.slug
  let status d = Lwt.return d.status
  let name d = Lwt.return d.name
  let kind d = Lwt.return d.kind
  let deviser d = Lwt.return d.deviser

  let equal dance1 dance2 =
    let%lwt slug1 = slug dance1 in
    let%lwt slug2 = slug dance2 in
    Lwt.return (Slug.equal slug1 slug2)
end
include Self

module Filter = struct
  let _key = "dance-filter"

  type predicate =
    | Is of t
    | Name of string
    | NameMatches of string
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]

  let is dance = Formula.pred (Is dance)
  let name name = Formula.pred (Name name)
  let nameMatches name = Formula.pred (NameMatches name)

  let raw = nameMatches

  let accepts filter dance =
    let char_equal = Char.Sensible.equal in
    Formula.interpret filter @@ function

    | Is dance' ->
      equal dance dance' >|=| Formula.interpret_bool

    | Name string ->
      let%lwt name = Self.name dance in
      Lwt.return (String.proximity ~char_equal string name)

    | NameMatches string ->
      let%lwt name = Self.name dance in
      Lwt.return (String.inclusion_proximity ~char_equal ~needle:string name)

  let nullary_text_predicates = []

  let unary_text_predicates =
    TextFormula.[
      "name",         raw_only ~convert:Fun.id name;
      "name-matches", raw_only ~convert:Fun.id nameMatches
    ]

  let from_text_formula =
    TextFormula.make_to_formula raw
      nullary_text_predicates
      unary_text_predicates
end
