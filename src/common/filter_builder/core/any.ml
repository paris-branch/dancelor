open Nes

(* NOTE: This [Raw] variant is a bit artificial, when we could already be
   inheriting the various [raw] cases, of the other filters. However, this
   would unfold text formulas into a big disjunction at the syntactic level,
   and we would rather avoid that. *)
type predicate =
  | Raw of string
  | Type of Model_builder.Core.Any.Type.t
  (* lifting predicates: *)
  | Source of Source.t
  | Person of (Model_builder.Core.Person.t, Person.t) Formula_entry.t
  | Dance of Dance.t
  | Book of Book.t
  | Set of Set.t
  | Tune of Tune.t
  | Version of Version.t
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

(** The [?human] flag specifies whether we should print eg. [Version
    <vfilter>] as ["type:version <vfilter>"]. This is correct but it will
    not generate the correct inverse of [of_string]. *)
let make_text_formula_converter ?(human = false) () =
  Text_formula_converter.(
    (* We find formulas of the form [type:version predicate-on-version]
       better for humans than [version:predicate-on-version]. *)
    let wrap_back = if human then Never else Always in
    merge
      ~tiebreaker: Left
      (
        (* Any-specific converter *)
        make
          [
            raw (ok % raw');
            unary_string ~name: "raw" (predicate_Raw, raw_val) ~wrap_back: Never;
            unary_raw ~name: "type" (type_, type__val) ~cast: (Model_builder.Core.Any.Type.of_string_opt, Model_builder.Core.Any.Type.to_string) ~type_: "valid type";
            unary_lift ~name: "is-source-such-that" (source, source_val) ~converter: Source.text_formula_converter ~wrap_back;
            unary_lift ~name: "is-person-such-that" (person, person_val) ~converter: (Formula_entry.text_formula_converter (Person.name' % Formula_string.matches') Person.text_formula_converter) ~wrap_back;
            unary_lift ~name: "is-dance-such-that" (dance, dance_val) ~converter: Dance.text_formula_converter ~wrap_back;
            unary_lift ~name: "is-book-such-that" (book, book_val) ~converter: Book.text_formula_converter ~wrap_back;
            unary_lift ~name: "is-set-such-that" (set, set_val) ~converter: Set.text_formula_converter ~wrap_back;
            unary_lift ~name: "is-tune-such-that" (tune, tune_val) ~converter: Tune.text_formula_converter ~wrap_back;
            unary_lift ~name: "is-version-such-that" (version, version_val) ~converter: Version.text_formula_converter ~wrap_back;
          ];
      )
      (
        merge_l
          [
            (* Other converters, lifted to Any *)
            map source Source.text_formula_converter ~error: ((^) "As source: ");
            map person (Formula_entry.text_formula_converter (Person.name' % Formula_string.matches') Person.text_formula_converter) ~error: ((^) "As person: ");
            map dance Dance.text_formula_converter ~error: ((^) "As dance: ");
            map book Book.text_formula_converter ~error: ((^) "As book: ");
            map set Set.text_formula_converter ~error: ((^) "As set: ");
            map tune Tune.text_formula_converter ~error: ((^) "As tune: ");
            map version Version.text_formula_converter ~error: ((^) "As version: ");
          ]
      )
  )
let text_formula_converter = make_text_formula_converter ()

let from_text_formula = Text_formula.to_formula text_formula_converter
let from_string ?filename input = Result.bind (Text_formula.from_string ?filename input) from_text_formula

let to_string = Text_formula.to_string % Text_formula.of_formula text_formula_converter

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

(* Little trick to convince OCaml that polymorphism is OK. *)
type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

let optimise =
  (* FIXME: Because of [type_based_cleanup], the following is not idempotent.
     Hence the call to [fixpoint] below. *)
  fixpoint
    (
      Formula.optimise
        ~binop: (fun {op} f1 f2 ->
          match (f1, f2) with
          (* [person:] eats [type:person] *)
          | (Type Source, Source f) | (Source f, Type Source) -> some @@ source f
          | (Type Person, Person f) | (Person f, Type Person) -> some @@ person f
          | (Type Dance, Dance f) | (Dance f, Type Dance) -> some @@ dance f
          | (Type Book, Book f) | (Book f, Type Book) -> some @@ book f
          | (Type Set, Set f) | (Set f, Type Set) -> some @@ set f
          | (Type Tune, Tune f) | (Tune f, Type Tune) -> some @@ tune f
          | (Type Version, Version f) | (Version f, Type Version) -> some @@ version f
          (* [person:<f1> ∧ person:<f2> -> person:(<f1> ∧ <f2>)] *)
          | (Source f1, Source f2) -> some @@ source (op f1 f2)
          | (Person f1, Person f2) -> some @@ person (op f1 f2)
          | (Dance f1, Dance f2) -> some @@ dance (op f1 f2)
          | (Book f1, Book f2) -> some @@ book (op f1 f2)
          | (Set f1, Set f2) -> some @@ set (op f1 f2)
          | (Tune f1, Tune f2) -> some @@ tune (op f1 f2)
          | (Version f1, Version f2) -> some @@ version (op f1 f2)
          | _ -> None
        )
        (function
          | (Raw _ as p) | (Type _ as p) -> p
          | Source pfilter -> source @@ Source.optimise pfilter
          | Person pfilter -> person @@ Formula_entry.optimise Person.optimise pfilter
          | Dance dfilter -> dance @@ Dance.optimise dfilter
          | Book bfilter -> book @@ Book.optimise bfilter
          | Set sfilter -> set @@ Set.optimise sfilter
          | Tune tfilter -> tune @@ Tune.optimise tfilter
          | Version vfilter -> version @@ Version.optimise vfilter
        ) %
        type_based_cleanup
    )

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
  Text_formula.to_string %
    Text_formula.of_formula (make_text_formula_converter ~human: true ()) %
    add_explicit_type %
    optimise

(* FIXME: once all the filters are entry-based, this version of specialise
   should go away *)
let specialise ~from_text_formula ~type_ ~unLift =
  Formula.convert @@ function
    | Raw str -> Result.get_ok (from_text_formula (Text_formula.raw' str))
    | Type t when Model_builder.Core.Any.Type.equal t type_ -> Formula.true_
    | Type _ -> Formula.false_
    | pred -> Option.value (unLift pred) ~default: Formula.false_

let specialise_entry ~from_text_formula ~type_ ~unLift =
  Formula.convert @@ function
    | Raw str -> Result.get_ok (from_text_formula (Text_formula.raw' str))
    | Type t when Model_builder.Core.Any.Type.equal t type_ -> Formula.true_
    | Type _ -> Formula.false_
    | pred -> Option.value (unLift pred) ~default: Formula.false_

let specialise formula = (
  specialise ~from_text_formula: Book.from_text_formula ~type_: Book ~unLift: book_val formula,
  specialise ~from_text_formula: Dance.from_text_formula ~type_: Dance ~unLift: dance_val formula,
  specialise ~from_text_formula: (Formula_entry.from_text_formula (Person.name' % Formula_string.matches') Person.text_formula_converter) ~type_: Person ~unLift: person_val formula,
  specialise ~from_text_formula: Set.from_text_formula ~type_: Set ~unLift: set_val formula,
  specialise ~from_text_formula: Source.from_text_formula ~type_: Source ~unLift: source_val formula,
  specialise ~from_text_formula: Tune.from_text_formula ~type_: Tune ~unLift: tune_val formula,
  specialise ~from_text_formula: Version.from_text_formula ~type_: Version ~unLift: version_val formula
)
