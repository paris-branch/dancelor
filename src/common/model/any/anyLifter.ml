open Nes

module Lift
    (Person : module type of PersonSignature)
    (Dance : module type of DanceSignature)
    (Book : module type of BookSignature)
    (Set : module type of SetSignature)
    (Tune : module type of TuneSignature)
    (Version : module type of VersionSignature)
= struct
  include AnyCore

  let equal any1 any2 =
    match any1, any2 with
    |  Person c1,  Person c2 -> Person.equal c1 c2
    |   Dance d1,   Dance d2 -> Dance.equal d1 d2
    |    Book b1,    Book b2 -> Book.equal b1 b2
    |     Set s1,     Set s2 -> Set.equal s1 s2
    |    Tune t1,    Tune t2 -> Tune.equal t1 t2
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
    include AnyCore.Type

    module Set = Stdlib.Set.Make(struct
        type nonrec t = t
        let compare = compare
      end)

    let all = [ Person; Dance; Book; Set; Tune; Version ]
    let all_s = Set.of_list all

    let equal = (=)

    let to_string = function
      | Person  -> "Person"
      | Dance   -> "Dance"
      | Book    -> "Book"
      | Set     -> "Set"
      | Tune    -> "Tune"
      | Version -> "Version"

    exception NotAType of string

    let of_string str =
      match String.lowercase_ascii str with
      | "person"  -> Person
      | "dance"   -> Dance
      | "book"    -> Book
      | "set"     -> Set
      | "tune"    -> Tune
      | "version" -> Version
      | _ -> raise (NotAType str)

    let of_string_opt str =
      try Some (of_string str)
      with NotAType _ -> None
  end

  let type_of = function
    |  Person _ -> Type.Person
    |   Dance _ -> Type.Dance
    |    Book _ -> Type.Book
    |     Set _ -> Type.Set
    |    Tune _ -> Type.Tune
    | Version _ -> Type.Version

  module Filter = struct
    include AnyCore.Filter

    let accepts filter any =
      Formula.interpret filter @@ function
      | Type type_ ->
        Type.equal (type_of any) type_
        |> Formula.interpret_bool
        |> Lwt.return

      | AsPerson cfilter ->
        (match any with
         | Person person -> Person.Filter.accepts cfilter person
         | _ -> Lwt.return Formula.interpret_false)

      | AsDance dfilter ->
        (match any with
         | Dance dance -> Dance.Filter.accepts dfilter dance
         | _ -> Lwt.return Formula.interpret_false)

      | AsBook bfilter ->
        (match any with
         | Book book -> Book.Filter.accepts bfilter book
         | _ -> Lwt.return Formula.interpret_false)

      | AsSet sfilter ->
        (match any with
         | Set set -> Set.Filter.accepts sfilter set
         | _ -> Lwt.return Formula.interpret_false)

      | AsTune tfilter ->
        (match any with
         | Tune tune -> Tune.Filter.accepts tfilter tune
         | _ -> Lwt.return Formula.interpret_false)

      | AsVersion vfilter ->
        (match any with
         | Version version -> Version.Filter.accepts vfilter version
         | _ -> Lwt.return Formula.interpret_false)

    let type_ type_ = Formula.pred (Type type_)

    let asPerson  filter = Formula.pred (AsPerson  filter)
    let asDance   filter = Formula.pred (AsDance   filter)
    let asBook    filter = Formula.pred (AsBook    filter)
    let asSet     filter = Formula.pred (AsSet     filter)
    let asTune    filter = Formula.pred (AsTune    filter)
    let asVersion filter = Formula.pred (AsVersion filter)

    let text_formula_converter =
      TextFormulaConverter.(
        merge_l [
          (* Any-specific converter *)
          make
            [
              unary_raw ~name:"type" (fun string ->
                  Result.map type_ @@
                  Option.to_result (Type.of_string_opt string)
                    ~none:(spf "Unary predicate \"type\" does not accept \"%s\" as a valid type" string)
                )
            ]
            ~raw: Result.error;
          (* Other converters, lifted to Any *)
          map asPerson Person.Filter.text_formula_converter;
          map asDance Dance.Filter.text_formula_converter;
          map asBook Book.Filter.text_formula_converter;
          map asSet Set.Filter.text_formula_converter;
          map asTune Tune.Filter.text_formula_converter;
          map asVersion Version.Filter.text_formula_converter;
        ]
      )

    let from_text_formula = TextFormula.to_formula text_formula_converter
    let from_string ?filename input =
      Result.map_error List.singleton @@ Result.bind (TextFormula.from_string ?filename input) from_text_formula

    let possible_types =
      let open Formula in
      let rec possible_types = function
        | False -> Type.Set.empty
        | True -> Type.all_s
        | Not formula -> Type.Set.diff Type.all_s (possible_types formula)
        | And (formula1, formula2) -> Type.Set.inter (possible_types formula1) (possible_types formula2)
        | Or  (formula1, formula2) -> Type.Set.union (possible_types formula1) (possible_types formula2)
        | Pred pred ->
          (* FIXME: We could do better here by checking in depth whether a formula
             has a chance to return. That would eliminate some other types. *)
          match pred with
          | Type type_  -> Type.Set.singleton type_
          | AsPerson  _ -> Type.Set.singleton Person
          | AsDance   _ -> Type.Set.singleton Dance
          | AsBook    _ -> Type.Set.singleton Book
          | AsSet     _ -> Type.Set.singleton Set
          | AsTune    _ -> Type.Set.singleton Tune
          | AsVersion _ -> Type.Set.singleton Version
      in
      List.of_seq % Type.Set.to_seq % possible_types
  end
end
