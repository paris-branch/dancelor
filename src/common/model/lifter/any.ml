open Nes
open Dancelor_common_database
open Dancelor_common_model_utils

module Lift
    (Person : module type of Dancelor_common_model_signature.Person)
    (Dance : module type of Dancelor_common_model_signature.Dance)
    (Book : module type of Dancelor_common_model_signature.Book)
    (Set : module type of Dancelor_common_model_signature.Set)
    (Tune : module type of Dancelor_common_model_signature.Tune)
    (Version : module type of Dancelor_common_model_signature.Version)
= struct
  include Dancelor_common_model_core.Any

  let equal any1 any2 =
    match any1, any2 with
    | Person c1, Person c2 -> Entry.equal' c1 c2
    | Dance d1, Dance d2 -> Dance.equal d1 d2
    | Book b1, Book b2 -> Book.equal b1 b2
    | Set s1, Set s2 -> Set.equal s1 s2
    | Tune t1, Tune t2 -> Tune.equal t1 t2
    | Version v1, Version v2 -> Version.equal v1 v2
    | _ -> false

  let name = function
    | Person p -> Lwt.return @@ Person.name p
    | Dance d -> Lwt.return @@ Dance.name d
    | Book b -> Lwt.return @@ Book.title b
    | Set s -> Lwt.return @@ Set.name s
    | Tune t -> Lwt.return @@ Tune.name t
    | Version v -> Version.name v

  module Type = struct
    include Dancelor_common_model_core.Any.Type

    let all = [Person; Dance; Book; Set; Tune; Version]

    module Set = struct
      include Stdlib.Set.Make(struct
          type nonrec t = t
          let compare = compare
        end)

      let all = of_list all
      let comp = diff all
    end

    let are_all l = Set.(equal (of_list l) all)

    let equal = (=)

    let to_string = function
      | Person -> "Person"
      | Dance -> "Dance"
      | Book -> "Book"
      | Set -> "Set"
      | Tune -> "Tune"
      | Version -> "Version"

    exception NotAType of string

    let of_string str =
      match String.lowercase_ascii str with
      | "person" -> Person
      | "dance" -> Dance
      | "book" -> Book
      | "set" -> Set
      | "tune" -> Tune
      | "version" -> Version
      | _ -> raise (NotAType str)

    let of_string_opt str =
      try
        Some (of_string str)
      with
      | NotAType _ -> None
  end

  let type_of = function
    | Person _ -> Type.Person
    | Dance _ -> Type.Dance
    | Book _ -> Type.Book
    | Set _ -> Type.Set
    | Tune _ -> Type.Tune
    | Version _ -> Type.Version

  module Filter = struct
    include Dancelor_common_model_filter.Any

    let rec accepts filter any =
      Formula.interpret filter @@ function
      | Raw string ->
        let lift_raw lift from_text_formula str =
          lift (Result.get_ok (from_text_formula (TextFormula.raw' str)))
        in
        Fun.flip accepts any @@
        Formula.or_l
          [
            lift_raw person' Person.Filter.from_text_formula string;
            lift_raw dance' Dance.Filter.from_text_formula string;
            lift_raw book' Book.Filter.from_text_formula string;
            lift_raw set' Set.Filter.from_text_formula string;
            lift_raw tune' Tune.Filter.from_text_formula string;
            lift_raw version' Version.Filter.from_text_formula string;
          ]
      | Type type_ ->
        Type.equal (type_of any) type_
        |> Formula.interpret_bool
        |> Lwt.return
      | Person cfilter ->
        (
          match any with
          | Person person -> Person.Filter.accepts cfilter person
          | _ -> Lwt.return Formula.interpret_false
        )
      | Dance dfilter ->
        (
          match any with
          | Dance dance -> Dance.Filter.accepts dfilter dance
          | _ -> Lwt.return Formula.interpret_false
        )
      | Book bfilter ->
        (
          match any with
          | Book book -> Book.Filter.accepts bfilter book
          | _ -> Lwt.return Formula.interpret_false
        )
      | Set sfilter ->
        (
          match any with
          | Set set -> Set.Filter.accepts sfilter set
          | _ -> Lwt.return Formula.interpret_false
        )
      | Tune tfilter ->
        (
          match any with
          | Tune tune -> Tune.Filter.accepts tfilter tune
          | _ -> Lwt.return Formula.interpret_false
        )
      | Version vfilter ->
        (
          match any with
          | Version version -> Version.Filter.accepts vfilter version
          | _ -> Lwt.return Formula.interpret_false
        )

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
                raw (Result.ok % Dancelor_common_model_filter.Any.raw');
                unary_string ~name: "raw" (Dancelor_common_model_filter.Any.raw, unRaw) ~wrap_back: Never;
                unary_raw ~name: "type" (type_, unType) ~cast: (Type.of_string_opt, Type.to_string) ~type_: "valid type";
                unary_lift ~name: "person" (person, unPerson) ~converter: Person.Filter.text_formula_converter ~wrap_back;
                unary_lift ~name: "dance" (dance, unDance) ~converter: Dance.Filter.text_formula_converter ~wrap_back;
                unary_lift ~name: "book" (book, unBook) ~converter: Book.Filter.text_formula_converter ~wrap_back;
                unary_lift ~name: "set" (set, unSet) ~converter: Set.Filter.text_formula_converter ~wrap_back;
                unary_lift ~name: "tune" (tune, unTune) ~converter: Tune.Filter.text_formula_converter ~wrap_back;
                unary_lift ~name: "version" (version, unVersion) ~converter: Version.Filter.text_formula_converter ~wrap_back;
              ];
          )
          (
            merge_l
              [
                (* Other converters, lifted to Any *)
                map person Person.Filter.text_formula_converter ~error: ((^) "As person: ");
                map dance Dance.Filter.text_formula_converter ~error: ((^) "As dance: ");
                map book Book.Filter.text_formula_converter ~error: ((^) "As book: ");
                map set Set.Filter.text_formula_converter ~error: ((^) "As set: ");
                map tune Tune.Filter.text_formula_converter ~error: ((^) "As tune: ");
                map version Version.Filter.text_formula_converter ~error: ((^) "As version: ");
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
        | Raw _ -> Type.Set.all
        | Type type_ -> Type.Set.singleton type_
        | Person _ -> Type.Set.singleton Person
        | Dance _ -> Type.Set.singleton Dance
        | Book _ -> Type.Set.singleton Book
        | Set _ -> Type.Set.singleton Set
        | Tune _ -> Type.Set.singleton Tune
        | Version _ -> Type.Set.singleton Version
      in
      let open Formula in
      (* Given a maximal set of possible types [t] and a formula, refine the
         possible types of the formula and a clean up the formula. The returned
         types are a subset of [t]. The returned formula does not contain
         predicates that would clash with [t]. *)
      let rec refine_types_and_cleanup t = function
        | False -> (Type.Set.empty, False)
        | True -> (t, True)
        | Not f ->
          (* REVIEW: Not 100% of this [Type.Set.comp t] argument. *)
          map_pair (Type.Set.diff t) not @@ refine_types_and_cleanup (Type.Set.comp t) f
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
          (Type.Set.union t1 t2, or_ f1 f2)
        | Pred p ->
          let ts = Type.Set.inter (types_of_predicate p) t in
          (ts, if Type.Set.is_empty ts then False else Pred p)
      in
      snd % refine_types_and_cleanup Type.Set.all

    (* Little trick to convince OCaml that polymorphism is OK. *)
    type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

    let optimise =
      let lift {op} f1 f2 =
        match (f1, f2) with
        (* [person:] eats [type:person] *)
        | (Type Person, Person f) | (Person f, Type Person) -> Option.some @@ person f
        | (Type Dance, Dance f) | (Dance f, Type Dance) -> Option.some @@ dance f
        | (Type Book, Book f) | (Book f, Type Book) -> Option.some @@ book f
        | (Type Set, Set f) | (Set f, Type Set) -> Option.some @@ set f
        | (Type Tune, Tune f) | (Tune f, Type Tune) -> Option.some @@ tune f
        | (Type Version, Version f) | (Version f, Type Version) -> Option.some @@ version f
        (* [person:<f1> ∧ person:<f2> -> person:(<f1> ∧ <f2>)] *)
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
              | Person pfilter -> person @@ Person.Filter.optimise pfilter
              | Dance dfilter -> dance @@ Dance.Filter.optimise dfilter
              | Book bfilter -> book @@ Book.Filter.optimise bfilter
              | Set sfilter -> set @@ Set.Filter.optimise sfilter
              | Tune tfilter -> tune @@ Tune.Filter.optimise tfilter
              | Version vfilter -> version @@ Version.Filter.optimise vfilter
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
  end
end
