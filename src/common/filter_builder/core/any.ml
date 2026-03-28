open Nes

(* NOTE: This [Raw] variant is a bit artificial, when we could already be
   inheriting the various [raw] cases, of the other filters. However, this
   would unfold text formulas into a big disjunction at the syntactic level,
   and we would rather avoid that. *)
type predicate =
  | Raw of string
  | Type of Model_builder.Core.Any.Type.t
  (* lifting predicates: *)
  | Source of (Model_builder.Core.Source.t, Source.t) Formula_entry.public
  | Person of (Model_builder.Core.Person.t, Person.t) Formula_entry.public
  | Dance of (Model_builder.Core.Dance.t, Dance.t) Formula_entry.public
  | Book of (Model_builder.Core.Book.t, Book.t) Formula_entry.private_
  | Set of (Model_builder.Core.Set.t, Set.t) Formula_entry.private_
  | Tune of (Model_builder.Core.Tune.t, Tune.t) Formula_entry.public
  | Version of (Model_builder.Core.Version.t, Version.t) Formula_entry.public
[@@deriving eq, show {with_path = false}, yojson, variants]

(* NOTE: To prevent some shadowing. *)
let predicate_Raw = raw

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let raw' = Formula.pred % raw
let type_' = Formula.pred % type_
let source' = Formula.pred % source
let person' = Formula.pred % person
let dance' = Formula.pred % dance
let book' = Formula.pred % book
let set' = Formula.pred % set
let tune' = Formula.pred % tune
let version' = Formula.pred % version

(** Clean up a formula by analysing the types of given predicates. For
    instance, ["type:version (version:key:A :or book::source)"] can be
    simplified to ["type:version version:key:A"]. *)
let type_based_cleanup =
  let module TypeSet = Model_builder.Core.Any.Type.Set in
  (* Returns the types of objects matched by a predicate. *)
  let types_of_predicate = function
    | Raw _ -> TypeSet.all
    | Type type_ -> TypeSet.singleton type_
    | Source _ -> TypeSet.singleton Source
    | Person _ -> TypeSet.singleton Person
    | Dance _ -> TypeSet.singleton Dance
    | Book _ -> TypeSet.singleton Book
    | Set _ -> TypeSet.singleton Set
    | Tune _ -> TypeSet.singleton Tune
    | Version _ -> TypeSet.singleton Version
  in
  let open Formula in
  (* Given a formula, a maximal set of possible types [t], and a maximal set of
     possible types for the negation of the formula, refine the possible types
     of the formula and a clean it up. The returned types are a subset of [t].
     The returned formula does not contain predicates that would clash with
     [t]. *)
  let return (t, tn, f) = (t, tn, if TypeSet.is_empty t then False else if TypeSet.is_empty tn then True else f) in
  let rec refine_types_and_cleanup t tn f =
    return @@
      match f with
      | False -> (TypeSet.empty, tn, False)
      | True -> (t, TypeSet.empty, True)
      | Not f ->
        let (tn, t, f) = refine_types_and_cleanup tn t f in
          (t, tn, Not f)
      | Or (f1, f2) ->
        let (t1, tn1, _) = refine_types_and_cleanup t tn f1 in
        let (t2, tn2, _) = refine_types_and_cleanup t tn f2 in
        let (t, tn) = (TypeSet.union t1 t2, TypeSet.inter tn1 tn2) in
        let (_, _, f1) = refine_types_and_cleanup t tn f1 in
        let (_, _, f2) = refine_types_and_cleanup t tn f2 in
          (t, tn, or_ f1 f2)
      | And (f1, f2) ->
        let (t1, tn1, _) = refine_types_and_cleanup t tn f1 in
        let (t2, tn2, _) = refine_types_and_cleanup t tn f2 in
        let (t, tn) = (TypeSet.inter t1 t2, TypeSet.union tn1 tn2) in
        let (_, _, f1) = refine_types_and_cleanup t tn f1 in
        let (_, _, f2) = refine_types_and_cleanup t tn f2 in
          (t, tn, and_ f1 f2)
      | Pred p ->
        let tp = types_of_predicate p in
        let t = TypeSet.inter t tp in
        let tn = match p with Type _ -> TypeSet.diff tn tp | _ -> tn in
          (t, tn, Pred p)
  in
  fun f ->
    let (_, _, f) = refine_types_and_cleanup TypeSet.all TypeSet.all f in
    f

let converter =
  Text_formula_converter.(
    make
      ~debug_name: "any"
      ~debug_print: pp_predicate
      ~raw: (ok % raw')
      ~lifters: (
        let lifter name (lift, unlift) converter typ =
          Text_formula_converter.lifter
            ~name: (spf "is-%s-such-that" name)
            (lift, unlift)
            converter
            ~down_not: (fun f -> some @@ Formula.(or_ (not (type_' typ)) (pred @@ lift (not f))))
            ~up_true: (type_' typ)
        in
        [
          lifter "source" (source, source_val) (Formula_entry.converter_public Source.converter) Source;
          lifter "person" (person, person_val) (Formula_entry.converter_public Person.converter) Person;
          lifter "dance" (dance, dance_val) (Formula_entry.converter_public Dance.converter) Dance;
          lifter "book" (book, book_val) (Formula_entry.converter_private Book.converter) Book;
          lifter "set" (set, set_val) (Formula_entry.converter_private Set.converter) Set;
          lifter "tune" (tune, tune_val) (Formula_entry.converter_public Tune.converter) Tune;
          lifter "version" (version, version_val) (Formula_entry.converter_public Version.converter) Version;
        ]
      )
      [unary_string ~name: "raw" (predicate_Raw, raw_val) ~wrap_back: Never;
      unary_raw ~name: "type" (type_, type__val) ~cast: (Model_builder.Core.Any.Type.of_string_opt, Model_builder.Core.Any.Type.to_string) ~type_: "valid type";
      ]
      ~pre_optimise: type_based_cleanup
  )

let specialise ~converter ~type_ ~unLift =
  Formula.convert @@ function
    | Raw str -> Result.get_ok (Text_formula_converter.raw converter str)
    | Type t when Model_builder.Core.Any.Type.equal t type_ -> Formula.true_
    | Type _ -> Formula.false_
    | pred -> Option.value (unLift pred) ~default: Formula.false_

let specialise formula = (
  specialise ~converter: (Formula_entry.converter_private Book.converter) ~type_: Book ~unLift: book_val formula,
  specialise ~converter: (Formula_entry.converter_public Dance.converter) ~type_: Dance ~unLift: dance_val formula,
  specialise ~converter: (Formula_entry.converter_public Person.converter) ~type_: Person ~unLift: person_val formula,
  specialise ~converter: (Formula_entry.converter_private Set.converter) ~type_: Set ~unLift: set_val formula,
  specialise ~converter: (Formula_entry.converter_public Source.converter) ~type_: Source ~unLift: source_val formula,
  specialise ~converter: (Formula_entry.converter_public Tune.converter) ~type_: Tune ~unLift: tune_val formula,
  specialise ~converter: (Formula_entry.converter_public Version.converter) ~type_: Version ~unLift: version_val formula
)
