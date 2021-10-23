open Nes

module Self = struct
  let _key = "person"

  type t =
    { slug : t Slug.t ;
      status : Status.t [@default Status.bot] ;
      name : string }
  [@@deriving yojson, make]

  let slug p = Lwt.return p.slug
  let status p = Lwt.return p.status
  let name p = Lwt.return p.name

  let equal person1 person2 =
    let%lwt slug1 = slug person1 in
    let%lwt slug2 = slug person2 in
    Lwt.return (Slug.equal slug1 slug2)
end
include Self

module Filter = struct
  let _key = "person-filter"

  type predicate =
    | Is of t
    | Name of string
    | NameMatches of string
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]

  let is person = Formula.pred (Is person)
  let name name = Formula.pred (Name name)
  let nameMatches name = Formula.pred (NameMatches name)

  let raw = nameMatches

  let accepts filter person =
    let char_equal = Char.Sensible.equal in
    Formula.interpret filter @@ function

    | Is person' ->
      equal person person' >|=| Formula.interpret_bool

    | Name string ->
      let%lwt name = Self.name person in
      Lwt.return (String.proximity ~char_equal string name)

    | NameMatches string ->
      let%lwt name = Self.name person in
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
