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

    let all = Set.of_list [ Person; Dance; Book; Set; Tune; Version ]

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

    let raw str =
      let filters = [
        (match  Person.Filter.raw str with Ok cfilter -> Ok ( asPerson cfilter) | Error err -> error_fmt "- for persons, %s"  err);
        (match   Dance.Filter.raw str with Ok dfilter -> Ok (  asDance dfilter) | Error err -> error_fmt "- for dances, %s"   err);
        (match    Book.Filter.raw str with Ok bfilter -> Ok (   asBook bfilter) | Error err -> error_fmt "- for books, %s"    err);
        (match     Set.Filter.raw str with Ok sfilter -> Ok (    asSet sfilter) | Error err -> error_fmt "- for sets, %s"     err);
        (match    Tune.Filter.raw str with Ok tfilter -> Ok (   asTune tfilter) | Error err -> error_fmt "- for tunes, %s"    err);
        (match Version.Filter.raw str with Ok vfilter -> Ok (asVersion vfilter) | Error err -> error_fmt "- for versions, %s" err);
      ] in
      let filters, errors =
        List.partition_map
          (function Ok filter -> Left filter | Error err -> Right err)
          filters
      in
      if filters <> [] then
        Ok (Formula.or_l filters)
      else
        match errors with
        | [] -> assert false
        | err :: _ -> Error err (* FIXME: also show the others *)

    let nullary_text_predicates = [
      "person",  type_ Type.Person;  (* alias for type:Person *)
      "dance",   type_ Type.Dance;   (* alias for type:Dance *)
      "book",    type_ Type.Book;    (* alias for type:Book *)
      "set",     type_ Type.Set;     (* alias for type:Set *)
      "tune",    type_ Type.Tune;    (* alias for type:Tune *)
      "version", type_ Type.Version; (* alias for type:Version *)
    ]

    let unary_text_predicates =
      TextFormula.[
        "type", raw_only ~convert:(fun s ->
            match Type.of_string_opt s with
            | Some t -> Ok t
            | None -> error_fmt ("There is an error in your request: "
                                 ^^ "\"%s\" is not a valid type.") s) type_;
      ]

    let from_text_formula =
      let from_text_predicate pred =
        let filters = [
          TextFormula.make_predicate_to_formula raw
            nullary_text_predicates unary_text_predicates pred;
          (match  Person.Filter.from_text_formula (Pred pred) with Ok cfilter -> Ok  (asPerson cfilter) | Error err -> error_fmt "- for persons, %s"  err);
          (match   Dance.Filter.from_text_formula (Pred pred) with Ok dfilter -> Ok   (asDance dfilter) | Error err -> error_fmt "- for dances, %s"   err);
          (match    Book.Filter.from_text_formula (Pred pred) with Ok bfilter -> Ok    (asBook bfilter) | Error err -> error_fmt "- for books, %s"    err);
          (match     Set.Filter.from_text_formula (Pred pred) with Ok sfilter -> Ok     (asSet sfilter) | Error err -> error_fmt "- for sets, %s"     err);
          (match    Tune.Filter.from_text_formula (Pred pred) with Ok tfilter -> Ok    (asTune tfilter) | Error err -> error_fmt "- for tunes, %s"    err);
          (match Version.Filter.from_text_formula (Pred pred) with Ok vfilter -> Ok (asVersion vfilter) | Error err -> error_fmt "- for versions, %s" err);
        ] in
        let filters, errors =
          List.partition_map
            (function Ok filter -> Left filter | Error err -> Right err)
            filters
        in
        if filters <> [] then
          Ok (Formula.or_l filters)
        else
          match errors with
          | [] -> assert false
          | _ ->
            Error ([
                "There is a part of your formula on which all types encountered an error.";
                aspf "The part in question is: %a." (TextFormula.Printer.pp_predicate False) pred;
                "The errors are:"
              ] @ errors)
      in
      TextFormula.to_formula from_text_predicate

    (** All the possible types that a formula can return. *)
    let rec possible_types =
      let open Formula in
      function
      | False -> Type.Set.empty
      | True -> Type.all
      | Not formula -> Type.Set.diff Type.all (possible_types formula)
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

    let nullary_text_predicates_of_type type_ =
      String.Set.of_list
        (match type_ with
         | Type.Person  -> List.map fst  Person.Filter.nullary_text_predicates
         | Type.Dance   -> List.map fst   Dance.Filter.nullary_text_predicates
         | Type.Book    -> List.map fst    Book.Filter.nullary_text_predicates
         | Type.Set     -> List.map fst     Set.Filter.nullary_text_predicates
         | Type.Tune    -> List.map fst    Tune.Filter.nullary_text_predicates
         | Type.Version -> List.map fst Version.Filter.nullary_text_predicates)

    let nullary_text_predicates_of_types types =
      Type.Set.fold
        (String.Set.union @@@ nullary_text_predicates_of_type)
        types (String.Set.of_list (List.map fst nullary_text_predicates))

    let unary_text_predicates_of_type type_ =
      String.Set.of_list
        (match type_ with
         | Type.Person  -> List.map fst  Person.Filter.unary_text_predicates
         | Type.Dance   -> List.map fst   Dance.Filter.unary_text_predicates
         | Type.Book    -> List.map fst    Book.Filter.unary_text_predicates
         | Type.Set     -> List.map fst     Set.Filter.unary_text_predicates
         | Type.Tune    -> List.map fst    Tune.Filter.unary_text_predicates
         | Type.Version -> List.map fst Version.Filter.unary_text_predicates)

    let unary_text_predicates_of_types types =
      Type.Set.fold
        (String.Set.union @@@ unary_text_predicates_of_type)
        types (String.Set.of_list (List.map fst unary_text_predicates))

    (* let check_predicates text_formula = *)

    exception UnknownPredicate of string * string

    let from_string string =
      try
        (
          let text_formula = TextFormula.from_string string in
          match from_text_formula text_formula with (* can yield parse error *)
          | Ok formula ->
            (
              let types_ = possible_types formula in
              (
                let unknown_nullary_text_predicates =
                  String.Set.diff
                    (TextFormula.nullary_predicates text_formula)
                    (nullary_text_predicates_of_types types_)
                in
                match String.Set.choose_opt unknown_nullary_text_predicates with
                | None -> ()
                | Some pred -> raise (UnknownPredicate ("nullary", pred))
              );
              (
                let unknown_unary_text_predicates =
                  String.Set.diff
                    (TextFormula.unary_predicates text_formula)
                    (unary_text_predicates_of_types types_)
                in
                match String.Set.choose_opt unknown_unary_text_predicates with
                | None -> ()
                | Some pred -> raise (UnknownPredicate ("unary", pred))
              );
              Ok formula
            )
          | Error err -> Error err
        )
      with
      | TextFormula.Lexer.UnexpectedCharacter char ->
        errors_fmt ("There is an unexpected character in your request: '%c'. "
                    ^^ "If you really want to type it, protect it with quotes, "
                    ^^ "eg. \"foo%cbar\".") char char
      | TextFormula.Lexer.UnterminatedQuote ->
        errors_fmt ("There is an unterminated quote in your request. "
                    ^^ "If you just want to type a quote character, "
                    ^^ "whether inside quotes or not, escape it, eg. \"foo\\\"bar\".")
      | TextFormula.Parser.ParseError (_, _, where) ->
        errors_fmt "There is a syntax error %s in your request." where
      | UnknownPredicate(arity, pred) ->
        (* FIXME: when building the predicate produces an error, it is considered
           unknown. This is shite. *)
        errors_fmt "The following %s predicate is either unknown or produces an error: \"%s\"." arity pred
      | exn ->
        errors_fmt ("Handling your request caused an unknown exception: %s. "
                    ^^ "Contact your system administrator with this message.")
          (Printexc.to_string exn)

    exception Error of string list

    let from_string_exn string =
      match from_string string with
      | Ok results -> results
      | Error err -> raise (Error err)
  end
end
