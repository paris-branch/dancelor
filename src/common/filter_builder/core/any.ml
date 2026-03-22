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

let converter =
  Text_formula_converter.(
    make
      ~debug_name: "any"
      ~debug_print: pp_predicate
      ~raw: (ok % raw')
      ~lifters: [
        lifter ~name: "is-source-such-that" (source, source_val) (Formula_entry.converter_public Source.converter) ~down_not: (fun f -> some @@ Formula.or_ (Not (type_' Source)) (source' (Not f)));
        lifter ~name: "is-person-such-that" (person, person_val) (Formula_entry.converter_public Person.converter) ~down_not: (fun f -> some @@ Formula.or_ (Not (type_' Person)) (person' (Not f)));
        lifter ~name: "is-dance-such-that" (dance, dance_val) (Formula_entry.converter_public Dance.converter) ~down_not: (fun f -> some @@ Formula.or_ (Not (type_' Dance)) (dance' (Not f)));
        lifter ~name: "is-book-such-that" (book, book_val) (Formula_entry.converter_private Book.converter) ~down_not: (fun f -> some @@ Formula.or_ (Not (type_' Book)) (book' (Not f)));
        lifter ~name: "is-set-such-that" (set, set_val) (Formula_entry.converter_private Set.converter) ~down_not: (fun f -> some @@ Formula.or_ (Not (type_' Set)) (set' (Not f)));
        lifter ~name: "is-tune-such-that" (tune, tune_val) (Formula_entry.converter_public Tune.converter) ~down_not: (fun f -> some @@ Formula.or_ (Not (type_' Tune)) (tune' (Not f)));
        lifter ~name: "is-version-such-that" (version, version_val) (Formula_entry.converter_public Version.converter) ~down_not: (fun f -> some @@ Formula.or_ (Not (type_' Version)) (version' (Not f)));
      ]
      [
        unary_string ~name: "raw" (predicate_Raw, raw_val) ~wrap_back: Never;
        unary_raw ~name: "type" (type_, type__val) ~cast: (Model_builder.Core.Any.Type.of_string_opt, Model_builder.Core.Any.Type.to_string) ~type_: "valid type";
      ];
  )

let from_text_formula = Text_formula.to_formula converter
let from_string ?filename input = Result.bind (Text_formula.from_string ?filename input) from_text_formula

let to_string = Text_formula.to_string % Text_formula.of_formula converter

(** Clean up a formula by analysing the types of given predicates. For
    instance, ["type:version (version:key:A :or book::source)"] can be
    simplified to ["type:version version:key:A"]. *)
let type_based_cleanup =
  (* Returns the types of objects matched by a predicate. *)
  let types_of_predicate = function
    | Raw _ -> Model_builder.Core.Any.Type.Set.all
    | Type type_ -> Model_builder.Core.Any.Type.Set.singleton type_
    | Source _ -> Model_builder.Core.Any.Type.Set.singleton Source
    | Person _ -> Model_builder.Core.Any.Type.Set.singleton Person
    | Dance _ -> Model_builder.Core.Any.Type.Set.singleton Dance
    | Book _ -> Model_builder.Core.Any.Type.Set.singleton Book
    | Set _ -> Model_builder.Core.Any.Type.Set.singleton Set
    | Tune _ -> Model_builder.Core.Any.Type.Set.singleton Tune
    | Version _ -> Model_builder.Core.Any.Type.Set.singleton Version
  in
  let open Formula in
  (* Given a maximal set of possible types [t] and a formula, refine the
     possible types of the formula and a clean up the formula. The returned
     types are a subset of [t]. The returned formula does not contain
     predicates that would clash with [t]. *)
  let rec refine_types_and_cleanup t = function
    | False -> (Model_builder.Core.Any.Type.Set.empty, False)
    | True -> (t, True)
    | Not f ->
      (* REVIEW: Not 100% of this [Type.Set.comp t] argument. *)
      Pair.map (Model_builder.Core.Any.Type.Set.diff t) not @@ refine_types_and_cleanup (Model_builder.Core.Any.Type.Set.comp t) f
    | And (f1, f2) ->
      (* Refine [t] on [f1], the refine it again while cleaning up [f2],
         then come back and clean up [f1]. *)
      let (t, _) = refine_types_and_cleanup t f1 in
      let (t, f2) = refine_types_and_cleanup t f2 in
      let (t, f1) = refine_types_and_cleanup t f1 in
        (t, and_ f1 f2)
    | Or (f1, f2) ->
      let (t1, f1) = refine_types_and_cleanup t f1 in
      let (t2, f2) = refine_types_and_cleanup t f2 in
        (Model_builder.Core.Any.Type.Set.union t1 t2, or_ f1 f2)
    | Pred p ->
      let ts = Model_builder.Core.Any.Type.Set.inter (types_of_predicate p) t in
        (ts, if Model_builder.Core.Any.Type.Set.is_empty ts then False else Pred p)
  in
  snd % refine_types_and_cleanup Model_builder.Core.Any.Type.Set.all

let optimise =
  (* FIXME: Because of [type_based_cleanup], the following is not idempotent.
     Hence the call to [fixpoint] below. *)
  fixpoint (Text_formula_converter.optimise converter % type_based_cleanup)

let to_pretty_string =
  let type_and t lift = function
    | Formula.True -> type_' t
    | f -> Formula.and_ (type_' t) (lift f)
  in
  let add_explicit_type =
    Formula.convert @@ function
      | Source f -> type_and Source source' f
      | Person f -> type_and Person person' f
      | Dance f -> type_and Dance dance' f
      | Book f -> type_and Book book' f
      | Set f -> type_and Set set' f
      | Tune f -> type_and Tune tune' f
      | Version f -> type_and Version version' f
      | p -> Formula.pred p
  in
  Text_formula.to_string % Text_formula.of_formula converter % add_explicit_type % optimise

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
