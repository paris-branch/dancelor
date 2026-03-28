open Nes

type predicate =
  | Source of (Model_builder.Core.Source.t, Source.t) Formula_entry.public
  | Person of (Model_builder.Core.Person.t, Person.t) Formula_entry.public
  | Dance of (Model_builder.Core.Dance.t, Dance.t) Formula_entry.public
  | Book of (Model_builder.Core.Book.t, Book.t) Formula_entry.private_
  | Set of (Model_builder.Core.Set.t, Set.t) Formula_entry.private_
  | Tune of (Model_builder.Core.Tune.t, Tune.t) Formula_entry.public
  | Version of (Model_builder.Core.Version.t, Version.t) Formula_entry.public
  | User of (Model_builder.Core.User.t, User.t) Formula_entry.public
  (* NOTE: This [Raw] variant is a bit artificial, when we could already be
     inheriting the various [raw] cases, of the other filters. However, this
     would unfold text formulas into a big disjunction at the syntactic level,
     and we would rather avoid that. *)
  (* NOTE: The [Raw] predicate appears last so as to be sorted last. *)
  | Raw of string
[@@deriving eq, ord, show {with_path = false}, yojson, variants]

(* NOTE: To prevent some shadowing. *)
let predicate_Raw = raw

type t = predicate Formula.t
[@@deriving eq, ord, show {with_path = false}, yojson]

let raw' = Formula.pred % raw
let source' = Formula.pred % source
let person' = Formula.pred % person
let dance' = Formula.pred % dance
let book' = Formula.pred % book
let set' = Formula.pred % set
let tune' = Formula.pred % tune
let version' = Formula.pred % version
let user' = Formula.pred % user

(** Given a predicate, return the set of types that this predicate's semantics
    may have. This might be an overapproximation; for instance. *)
let predicate_to_possible_types : predicate -> Model_builder.Core.Any.Type.Set.t =
  let open Model_builder.Core.Any.Type.Set in
  function
    | Raw _ -> all
    | Source _ -> singleton Source
    | Person _ -> singleton Person
    | Dance _ -> singleton Dance
    | Book _ -> singleton Book
    | Set _ -> singleton Set
    | Tune _ -> singleton Tune
    | Version _ -> singleton Version
    | User _ -> singleton User

(** Given a predicate, return the exact type that this predicate's semantics
    have. For instance, for [Source True], this is [Some Source], but for any
    other subformula of [Source _], this is [None]. *)
let predicate_to_exact_type : predicate -> Model_builder.Core.Any.Type.t option = function
  | Source True -> Some Source
  | Person True -> Some Person
  | Dance True -> Some Dance
  | Book True -> Some Book
  | Set True -> Some Set
  | Tune True -> Some Tune
  | Version True -> Some Version
  | User True -> Some User
  | _ -> None

(** Given a type, return the predicate whose semantics are exactly this type.
    For instance, for [Source], this is [Source True]. *)
let type_to_exact_predicate : Model_builder.Core.Any.Type.t -> predicate = function
  | Source -> source True
  | Person -> person True
  | Dance -> dance True
  | Book -> book True
  | Set -> set True
  | Tune -> tune True
  | Version -> version True
  | User -> user True

(** Clean up a formula by analysing the types of given predicates. For
    instance, ["type:version (version:key:A :or book::source)"] can be
    simplified to ["type:version version:key:A"]. *)
let type_based_cleanup =
  let module TypeSet = Model_builder.Core.Any.Type.Set in
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
        let tp = predicate_to_possible_types p in
        let t = TypeSet.inter t tp in
        let tn =
          match predicate_to_exact_type p with
          | Some tp -> TypeSet.remove tp tn
          | None -> tn
        in
          (t, tn, Pred p)
  in
  fun f ->
    let (_, _, f) = refine_types_and_cleanup TypeSet.all TypeSet.all f in
    f

let converter : predicate Text_formula_converter.t =
  Text_formula_converter.(
    make
      ~debug_name: "any"
      ~debug_print: pp_predicate
      ~raw: (ok % raw')
      ~lifters: (
        let lifter name (lift, unlift) converter typ =
          Text_formula_converter.lifter
            ~name: (spf "is-%s-such-that" name)
            ~inline: (
              Inline_custom {
                inline_text_formula = (
                  let type_constraint = Text_formula.(unary' "type" (raw' @@ String.capitalize_ascii name)) in
                  function
                    | True -> type_constraint
                    | f -> Formula.and_ type_constraint f
                );
                except_raw = true;
              }
            )
            (lift, unlift)
            converter
            ~up_true: None
            ~down_not: (function
              | True ->
                (* special case for the negation of exact predicates, otherwise this will loop *)
                some @@
                Formula.or_l @@
                List.map
                  (Formula.pred % type_to_exact_predicate)
                  Model_builder.Core.Any.Type.Set.(to_list @@ remove typ all)
              | f -> some @@ Formula.(or_ (not_ (pred @@ type_to_exact_predicate typ)) (pred @@ lift (not_ f)))
            )
        in
        [
          lifter "source" (source, source_val) (Formula_entry.converter_public Source.converter) Source;
          lifter "person" (person, person_val) (Formula_entry.converter_public Person.converter) Person;
          lifter "dance" (dance, dance_val) (Formula_entry.converter_public Dance.converter) Dance;
          lifter "book" (book, book_val) (Formula_entry.converter_private Book.converter) Book;
          lifter "set" (set, set_val) (Formula_entry.converter_private Set.converter) Set;
          lifter "tune" (tune, tune_val) (Formula_entry.converter_public Tune.converter) Tune;
          lifter "version" (version, version_val) (Formula_entry.converter_public Version.converter) Version;
          lifter "user" (user, user_val) (Formula_entry.converter_public User.converter) User;
        ]
      )
      [unary_string ~name: "raw" (predicate_Raw, raw_val) ~wrap_back: Never;
      unary_raw ~name: "type" (type_to_exact_predicate, const None) ~cast: Model_builder.Core.Any.Type.(of_string_opt, to_string) ~type_: "valid type";
      ]
      ~compare_predicate
      ~pre_optimise: type_based_cleanup
  )

let specialise ~converter ~unlift =
  Formula.convert @@ function
    | Raw str -> Result.get_ok (Text_formula_converter.raw converter str)
    | pred -> Option.value (unlift pred) ~default: Formula.false_

let specialise formula = (
  specialise ~converter: (Formula_entry.converter_private Book.converter) ~unlift: book_val formula,
  specialise ~converter: (Formula_entry.converter_public Dance.converter) ~unlift: dance_val formula,
  specialise ~converter: (Formula_entry.converter_public Person.converter) ~unlift: person_val formula,
  specialise ~converter: (Formula_entry.converter_private Set.converter) ~unlift: set_val formula,
  specialise ~converter: (Formula_entry.converter_public Source.converter) ~unlift: source_val formula,
  specialise ~converter: (Formula_entry.converter_public Tune.converter) ~unlift: tune_val formula,
  specialise ~converter: (Formula_entry.converter_public Version.converter) ~unlift: version_val formula
)
