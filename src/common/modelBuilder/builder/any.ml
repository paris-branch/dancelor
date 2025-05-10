open Nes

module Build
  (Book : Signature.Book.S)
  (Dance : Signature.Dance.S)
  (Person : Signature.Person.S)
  (Set : Signature.Set.S)
  (Source : Signature.Source.S)
  (Tune : Signature.Tune.S)
  (Version : Signature.Version.S)
= struct
  include Core.Any

  let equal any1 any2 =
    match any1, any2 with
    | Source c1, Source c2 -> Entry.equal' c1 c2
    | Person c1, Person c2 -> Entry.equal' c1 c2
    | Dance d1, Dance d2 -> Dance.equal d1 d2
    | Book b1, Book b2 -> Book.equal b1 b2
    | Set s1, Set s2 -> Set.equal s1 s2
    | Tune t1, Tune t2 -> Entry.equal Tune.equal t1 t2
    | Version v1, Version v2 -> Entry.equal Version.equal v1 v2
    | _ -> false

  let name = function
    | Source p -> Lwt.return @@ Source.name' p
    | Person p -> Lwt.return @@ Person.name' p
    | Dance d -> Lwt.return @@ Dance.name' d
    | Book b -> Lwt.return @@ Book.title' b
    | Set s -> Lwt.return @@ Set.name' s
    | Tune t -> Lwt.return @@ Tune.name' t
    | Version v -> Version.name' v

  module Type = Core.Any.Type

  module Filter = struct
    include Filter.Any

    let rec accepts filter any =
      Formula.interpret filter @@ function
        | Raw string ->
          let lift_raw lift from_text_formula str =
            lift (Result.get_ok (from_text_formula (TextFormula.raw' str)))
          in
          Fun.flip accepts any @@
            Formula.or_l
              [
                lift_raw source' Source.Filter.from_text_formula string;
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
        | Source cfilter ->
          (
            match any with
            | Source source -> Source.Filter.accepts cfilter source
            | _ -> Lwt.return Formula.interpret_false
          )
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
  end
end
