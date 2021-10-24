let _key = "any"

type t =
  | Credit of Credit.t
  | Dance of Dance.t
  | Person of Person.t
  | Book of Book.t
  | Set of Set.t
  (* | Source of Source.t *)
  | Tune of Tune.t
  | Version of Version.t
[@@deriving yojson]

let equal any1 any2 =
  match any1, any2 with
  | Credit c1,  Credit c2  -> Credit.equal  c1 c2
  | Dance d1,   Dance d2   -> Dance.equal   d1 d2
  | Person p1,  Person p2  -> Person.equal  p1 p2
  | Book b1,    Book b2    -> Book.equal    b1 b2
  | Set s1,     Set s2     -> Set.equal     s1 s2
  (* | Source s1,  Source s2  -> Source.equal  s1 s2 *)
  | Tune t1,    Tune t2    -> Tune.equal    t1 t2
  | Version v1, Version v2 -> Version.equal v1 v2
  | _ -> Lwt.return_false

module Type = struct
  let _key = "type"

  type t =
    | Credit
    | Dance
    | Person
    | Book
    | Set
    (* | Source *)
    | Tune
    | Version
  [@@deriving yojson]

  module Set = Stdlib.Set.Make(struct
      type nonrec t = t
      let compare = compare
    end)

  let all = Set.of_list [
      Credit; Dance; Person; Book;
      Set; (* Source; *) Tune; Version;
    ]

  let equal = (=)

  let to_string = function
    | Credit -> "Credit"
    | Dance -> "Dance"
    | Person -> "Person"
    | Book -> "Book"
    | Set -> "Set"
    (* | Source -> "Source" *)
    | Tune -> "Tune"
    | Version -> "Version"

  let of_string str =
    match String.lowercase_ascii str with
    | "credit" -> Credit
    | "dance" -> Dance
    | "person" -> Person
    | "book" -> Book
    | "set" -> Set
    (* | "source" -> Source *)
    | "tune" -> Tune
    | "version" -> Version
    | _ -> failwith "Any.Type.of_string"
end

let type_of = function
  | Credit _ -> Type.Credit
  | Dance _ -> Type.Dance
  | Person _ -> Type.Person
  | Book _ -> Type.Book
  | Set _ -> Type.Set
  (* | Source _ -> Type.Source *)
  | Tune _ -> Type.Tune
  | Version _ -> Type.Version

module Filter = struct
  let _key = "any-filter"

  type predicate =
    | Type of Type.t
    (* lifting predicates: *)
    | AsCredit  of  Credit.Filter.t
    | AsDance   of   Dance.Filter.t
    | AsPerson  of  Person.Filter.t
    | AsBook    of    Book.Filter.t
    | AsSet     of     Set.Filter.t
    (* | AsSource of Source.Filter.t *)
    | AsTune    of    Tune.Filter.t
    | AsVersion of Version.Filter.t
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]

  let type_ type_ = Formula.pred (Type type_)

  let asCredit  filter = Formula.pred (AsCredit  filter)
  let asDance   filter = Formula.pred (AsDance   filter)
  let asPerson  filter = Formula.pred (AsPerson  filter)
  let asBook    filter = Formula.pred (AsBook    filter)
  let asSet     filter = Formula.pred (AsSet     filter)
  (* let asSource filter = Formula.pred (AsSource filter) *)
  let asTune    filter = Formula.pred (AsTune    filter)
  let asVersion filter = Formula.pred (AsVersion filter)

  let raw str =
    Formula.or_l [
      asCredit   (Credit.Filter.raw str);
      asDance     (Dance.Filter.raw str);
      asPerson   (Person.Filter.raw str);
      asBook       (Book.Filter.raw str);
      asSet         (Set.Filter.raw str);
      (* asSource (Source.Filter.raw str); *)
      asTune       (Tune.Filter.raw str);
      asVersion (Version.Filter.raw str);
    ]

  let nullary_text_predicates = []

  let unary_text_predicates =
    TextFormula.[
      "type", raw_only ~convert:Type.of_string type_;
    ]

  let from_text_formula =
    let from_text_predicate pred =
      Formula.or_l [
        TextFormula.make_predicate_to_formula raw
          nullary_text_predicates unary_text_predicates pred;
        asCredit   (Credit.Filter.from_text_formula (Pred pred));
        asDance     (Dance.Filter.from_text_formula (Pred pred));
        asPerson   (Person.Filter.from_text_formula (Pred pred));
        asBook       (Book.Filter.from_text_formula (Pred pred));
        asSet         (Set.Filter.from_text_formula (Pred pred));
        (* asSource   (Source.Filter.from_text_formula (Pred pred)); *)
        asTune       (Tune.Filter.from_text_formula (Pred pred));
        asVersion (Version.Filter.from_text_formula (Pred pred));
      ]
    in
    TextFormula.to_formula from_text_predicate

  (** All the possible types that a formula can return. *)
  let rec possible_types =
    let open Formula in function
      | False -> Type.Set.empty
      | True -> Type.all
      | Not formula ->
        Type.Set.diff
          Type.all
          (possible_types formula)
      | And (formula1, formula2) ->
        Type.Set.inter
          (possible_types formula1)
          (possible_types formula2)
      | Or (formula1, formula2) ->
        Type.Set.union
          (possible_types formula1)
          (possible_types formula2)
      | Pred pred ->
        (* FIXME: We could do better here by checking in depth whether a formula
           has a chance to return. That would eliminate some other types. *)
        match pred with
        | Type type_  -> Type.Set.singleton type_
        | AsCredit  _ -> Type.Set.singleton Credit
        | AsDance   _ -> Type.Set.singleton Dance
        | AsPerson  _ -> Type.Set.singleton Person
        | AsBook    _ -> Type.Set.singleton Book
        | AsSet     _ -> Type.Set.singleton Set
        (* | AsSource _ ->   Type.Set.singleton Source *)
        | AsTune    _ -> Type.Set.singleton Tune
        | AsVersion _ -> Type.Set.singleton Version
end
