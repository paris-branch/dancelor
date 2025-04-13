open Nes

(* NOTE: This [Raw] variant is a bit artificial, when we could already be
   inheriting the various [raw] cases, of the other filters. However, this
   would unfold text formulas into a big disjunction at the syntactic level,
   and we would rather avoid that. *)
type predicate =
  | Raw of string
  | Type of Core.Any.Type.t
  (* lifting predicates: *)
  | Source of Source.t
  | Person of Person.t
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
  TextFormulaConverter.(
    (* We find formulas of the form [type:version predicate-on-version]
       better for humans than [version:predicate-on-version]. *)
    let wrap_back = if human then Never else Always in
    merge
      ~tiebreaker: Left
      (
        (* Any-specific converter *)
        make
          [
            raw (Result.ok % raw');
            unary_string ~name: "raw" (predicate_Raw, unRaw) ~wrap_back: Never;
            unary_raw ~name: "type" (type_, unType) ~cast: (Core.Any.Type.of_string_opt, Core.Any.Type.to_string) ~type_: "valid type";
            unary_lift ~name: "source" (source, unSource) ~converter: Source.text_formula_converter ~wrap_back;
            unary_lift ~name: "person" (person, unPerson) ~converter: Person.text_formula_converter ~wrap_back;
            unary_lift ~name: "dance" (dance, unDance) ~converter: Dance.text_formula_converter ~wrap_back;
            unary_lift ~name: "book" (book, unBook) ~converter: Book.text_formula_converter ~wrap_back;
            unary_lift ~name: "set" (set, unSet) ~converter: Set.text_formula_converter ~wrap_back;
            unary_lift ~name: "tune" (tune, unTune) ~converter: Tune.text_formula_converter ~wrap_back;
            unary_lift ~name: "version" (version, unVersion) ~converter: Version.text_formula_converter ~wrap_back;
          ];
      )
      (
        merge_l
          [
            (* Other converters, lifted to Any *)
            map source Source.text_formula_converter ~error: ((^) "As source: ");
            map person Person.text_formula_converter ~error: ((^) "As person: ");
            map dance Dance.text_formula_converter ~error: ((^) "As dance: ");
            map book Book.text_formula_converter ~error: ((^) "As book: ");
            map set Set.text_formula_converter ~error: ((^) "As set: ");
            map tune Tune.text_formula_converter ~error: ((^) "As tune: ");
            map version Version.text_formula_converter ~error: ((^) "As version: ");
          ]
      )
  )
let text_formula_converter = make_text_formula_converter ()

let from_text_formula = TextFormula.to_formula text_formula_converter
let from_string ?filename input = Result.bind (TextFormula.from_string ?filename input) from_text_formula

let to_string = TextFormula.to_string % TextFormula.of_formula text_formula_converter

(** Clean up a formula by analysing the types of given predicates. For
    instance, ["type:version (version:key:A :or book::source)"] can be
    simplified to ["type:version version:key:A"]. *)
let type_based_cleanup =
  (* Returns the types of objects matched by a predicate. *)
  let types_of_predicate = function
    | Raw _ -> Core.Any.Type.Set.all
    | Type type_ -> Core.Any.Type.Set.singleton type_
    | Source _ -> Core.Any.Type.Set.singleton Source
    | Person _ -> Core.Any.Type.Set.singleton Person
    | Dance _ -> Core.Any.Type.Set.singleton Dance
    | Book _ -> Core.Any.Type.Set.singleton Book
    | Set _ -> Core.Any.Type.Set.singleton Set
    | Tune _ -> Core.Any.Type.Set.singleton Tune
    | Version _ -> Core.Any.Type.Set.singleton Version
  in
  let open Formula in
  (* Given a maximal set of possible types [t] and a formula, refine the
     possible types of the formula and a clean up the formula. The returned
     types are a subset of [t]. The returned formula does not contain
     predicates that would clash with [t]. *)
  let rec refine_types_and_cleanup t = function
    | False -> (Core.Any.Type.Set.empty, False)
    | True -> (t, True)
    | Not f ->
      (* REVIEW: Not 100% of this [Type.Set.comp t] argument. *)
      map_pair (Core.Any.Type.Set.diff t) not @@ refine_types_and_cleanup (Core.Any.Type.Set.comp t) f
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
      (Core.Any.Type.Set.union t1 t2, or_ f1 f2)
    | Pred p ->
      let ts = Core.Any.Type.Set.inter (types_of_predicate p) t in
      (ts, if Core.Any.Type.Set.is_empty ts then False else Pred p)
  in
  snd % refine_types_and_cleanup Core.Any.Type.Set.all

(* Little trick to convince OCaml that polymorphism is OK. *)
type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

let optimise =
  let lift {op} f1 f2 =
    match (f1, f2) with
    (* [person:] eats [type:person] *)
    | (Type Source, Source f) | (Source f, Type Source) -> Option.some @@ source f
    | (Type Person, Person f) | (Person f, Type Person) -> Option.some @@ person f
    | (Type Dance, Dance f) | (Dance f, Type Dance) -> Option.some @@ dance f
    | (Type Book, Book f) | (Book f, Type Book) -> Option.some @@ book f
    | (Type Set, Set f) | (Set f, Type Set) -> Option.some @@ set f
    | (Type Tune, Tune f) | (Tune f, Type Tune) -> Option.some @@ tune f
    | (Type Version, Version f) | (Version f, Type Version) -> Option.some @@ version f
    (* [person:<f1> ∧ person:<f2> -> person:(<f1> ∧ <f2>)] *)
    | (Source f1, Source f2) -> Option.some @@ source (op f1 f2)
    | (Person f1, Person f2) -> Option.some @@ person (op f1 f2)
    | (Dance f1, Dance f2) -> Option.some @@ dance (op f1 f2)
    | (Book f1, Book f2) -> Option.some @@ book (op f1 f2)
    | (Set f1, Set f2) -> Option.some @@ set (op f1 f2)
    | (Tune f1, Tune f2) -> Option.some @@ tune (op f1 f2)
    | (Version f1, Version f2) -> Option.some @@ version (op f1 f2)
    | _ -> None
  in
  fixpoint
    (
      (* FIXME: Because of [type_based_cleanup], the following is not
         idempotent. Hence the [fixpoint] above. *)
      Formula.optimise
        ~lift_and: (lift {op = Formula.and_})
        ~lift_or: (lift {op = Formula.or_})
        (function
          | (Raw _ as p) | (Type _ as p) -> p
          | Source pfilter -> source @@ Source.optimise pfilter
          | Person pfilter -> person @@ Person.optimise pfilter
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
  TextFormula.to_string %
  TextFormula.of_formula (make_text_formula_converter ~human: true ()) %
  add_explicit_type %
  optimise
