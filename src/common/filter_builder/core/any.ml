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

(** Custom [wrap_back] that serialises [Source(f)] as [type:source f] instead of
    the verbose [is-source-such-that:f]. *)
let type_wrap type_name =
  let type_str = Model_builder.Core.Any.Type.to_string type_name in
  Text_formula_converter.Custom (fun tf ->
    let type_tf = Text_formula_type.unary' "type" (Text_formula_type.raw' type_str) in
    match tf with
    | Formula.True -> type_tf
    | _ -> Formula.and_ type_tf tf
  )

let converter =
  Text_formula_converter.(
    (* We find formulas of the form [type:version predicate-on-version]
       better for humans than [version:predicate-on-version]. *)
    merge
      ~tiebreaker: Left
      (
        (* Any-specific converter *)
        make
          ~raw: (ok % raw')
          [
            unary_string ~name: "raw" (predicate_Raw, raw_val) ~wrap_back: Never;
            unary_raw ~name: "type" (type_, type__val) ~cast: (Model_builder.Core.Any.Type.of_string_opt, Model_builder.Core.Any.Type.to_string) ~type_: "valid type";
            unary_lift ~name: "is-source-such-that" (source, source_val) ~converter: (Formula_entry.converter_public Source.converter) ~wrap_back: (type_wrap Source);
            unary_lift ~name: "is-person-such-that" (person, person_val) ~converter: (Formula_entry.converter_public Person.converter) ~wrap_back: (type_wrap Person);
            unary_lift ~name: "is-dance-such-that" (dance, dance_val) ~converter: (Formula_entry.converter_public Dance.converter) ~wrap_back: (type_wrap Dance);
            unary_lift ~name: "is-book-such-that" (book, book_val) ~converter: (Formula_entry.converter_private Book.converter) ~wrap_back: (type_wrap Book);
            unary_lift ~name: "is-set-such-that" (set, set_val) ~converter: (Formula_entry.converter_private Set.converter) ~wrap_back: (type_wrap Set);
            unary_lift ~name: "is-tune-such-that" (tune, tune_val) ~converter: (Formula_entry.converter_public Tune.converter) ~wrap_back: (type_wrap Tune);
            unary_lift ~name: "is-version-such-that" (version, version_val) ~converter: (Formula_entry.converter_public Version.converter) ~wrap_back: (type_wrap Version);
          ];
      )
      (
        merge_l
          [
            (* Other converters, lifted to Any *)
            map source (Formula_entry.converter_public Source.converter) ~error: ((^) "As source: ");
            map person (Formula_entry.converter_public Person.converter) ~error: ((^) "As person: ");
            map dance (Formula_entry.converter_public Dance.converter) ~error: ((^) "As dance: ");
            map book (Formula_entry.converter_private Book.converter) ~error: ((^) "As book: ");
            map set (Formula_entry.converter_private Set.converter) ~error: ((^) "As set: ");
            map tune (Formula_entry.converter_public Tune.converter) ~error: ((^) "As tune: ");
            map version (Formula_entry.converter_public Version.converter) ~error: ((^) "As version: ");
          ]
      )
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

(* Little trick to convince OCaml that polymorphism is OK. *)
type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

let optimise =
  (* FIXME: Because of [type_based_cleanup], the following is not idempotent.
     Hence the call to [fixpoint] below. *)
  fixpoint
    (
      Formula.optimise
        ~up_false: (fun {is_false} ->
          function
            | Source f when is_false f -> some Formula.false_
            | Person f when is_false f -> some Formula.false_
            | Dance f when is_false f -> some Formula.false_
            | Book f when is_false f -> some Formula.false_
            | Set f when is_false f -> some Formula.false_
            | Tune f when is_false f -> some Formula.false_
            | Version f when is_false f -> some Formula.false_
            | _ -> None
        )
        ~up_true: (fun {is_true} ->
          function
            | Source f when is_true f -> some @@ type_' Source
            | Person f when is_true f -> some @@ type_' Person
            | Dance f when is_true f -> some @@ type_' Dance
            | Book f when is_true f -> some @@ type_' Book
            | Set f when is_true f -> some @@ type_' Set
            | Tune f when is_true f -> some @@ type_' Tune
            | Version f when is_true f -> some @@ type_' Version
            | _ -> None
        )
        ~not_: (function
          | Source f -> some @@ source @@ Formula.not f
          | Person f -> some @@ person @@ Formula.not f
          | Dance f -> some @@ dance @@ Formula.not f
          | Book f -> some @@ book @@ Formula.not f
          | Set f -> some @@ set @@ Formula.not f
          | Tune f -> some @@ tune @@ Formula.not f
          | Version f -> some @@ version @@ Formula.not f
          | _ -> None
        )
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
          | Source pfilter -> source @@ Formula_entry.optimise_public Source.optimise pfilter
          | Person pfilter -> person @@ Formula_entry.optimise_public Person.optimise pfilter
          | Dance dfilter -> dance @@ Formula_entry.optimise_public Dance.optimise dfilter
          | Book bfilter -> book @@ Formula_entry.optimise_private Book.optimise bfilter
          | Set sfilter -> set @@ Formula_entry.optimise_private Set.optimise sfilter
          | Tune tfilter -> tune @@ Formula_entry.optimise_public Tune.optimise tfilter
          | Version vfilter -> version @@ Formula_entry.optimise_public Version.optimise vfilter
        ) %
        type_based_cleanup
    )

let to_pretty_string = to_string % optimise

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
