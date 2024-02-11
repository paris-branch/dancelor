open QCheck
module Model = Dancelor_common_model

module Slug = struct
  type 'any t = [%import: 'any NesSlug.t]

  let gen _ =
    Gen.map
      NesSlug.unsafe_of_string
      Gen.(string_size ~gen:(char_range 'a' 'z') (int_range 1 10))

  let shrink slug =
    if not (NesSlug.is_none slug) && NesSlug.to_string slug = "a" then
      Iter.empty
    else
      Iter.return (NesSlug.unsafe_of_string "a")
end


module Music = struct
  (* FIXME: Should be able to go in the [@@deriving] block. *)

  type note = [%import: Dancelor_common_model.Music.note] [@@deriving qcheck]
  type alteration = [%import: Dancelor_common_model.Music.alteration] [@@deriving qcheck]

  type octave = [%import: Dancelor_common_model.Music.octave]
  let gen_octave = Gen.int_range (-8) 8
  let shrink_octave = function
    | 0 -> Iter.empty
    | _ -> Iter.return 0

  type pitch = [%import: Dancelor_common_model.Music.pitch] [@@deriving qcheck]
  type mode = [%import: Dancelor_common_model.Music.mode] [@@deriving qcheck]
  type key = [%import: Dancelor_common_model.Music.key] [@@deriving qcheck]
  type clef = [%import: Dancelor_common_model.Music.clef] [@@deriving qcheck]
end

module Formula = struct
  type 'p t = [%import: 'p Dancelor_common_model.Formula.t] [@@deriving qcheck]

  (* Formulas can grow very quickly. We therefore limit the size of formulas
     drastically in our generator. *)
  let gen gen_p = gen_sized gen_p 10

  let rec shrink shrink_predicate = let open Iter in function
      | False -> empty
      | True -> empty
      | Not f -> return f
      | And (f1, f2) -> (return f1) <+> (return f2)
                        <+> map (fun f1' -> And (f1', f2)) (shrink shrink_predicate f1)
                        <+> map (fun f2' -> And (f1, f2')) (shrink shrink_predicate f2)
      | Or (f1, f2) -> (return f1) <+> (return f2)
                       <+> map (fun f1' -> Or (f1', f2)) (shrink shrink_predicate f1)
                       <+> map (fun f2' -> Or (f1, f2')) (shrink shrink_predicate f2)
      | Pred p -> map (fun p' -> Pred p') (shrink_predicate p)
end

module TextFormula = struct
  type predicate = [%import: Dancelor_common_model.TextFormula.predicate]
  and t = [%import: Dancelor_common_model.TextFormula.t]

  let gen_predicate_name =
    let open Gen in
    fix
      (fun self () ->
         let* name = string_size ~gen:(char_range 'a' 'z') (int_range 1 10) in
         if name = "or" || name = "and" || name = "not" then
           self ()
         else
           pure name)
      ()

  let gen_predicate =
    let open Gen in
    fix
      (fun self () ->
         oneof [
           (Dancelor_common_model.TextFormula.raw <$> string_printable);
           (Dancelor_common_model.TextFormula.nullary <$> gen_predicate_name);
           (Dancelor_common_model.TextFormula.unary <$> gen_predicate_name <*> Formula.gen (self ()));
         ]
      )
      ()

  let gen = Formula.gen gen_predicate

  let shrink' _ = Iter.empty
end

module Kind = struct
  module Base = struct
    type t = [%import: Dancelor_common_model.Kind.Base.t] [@@deriving qcheck]

    module Filter = struct
      type predicate = [%import: Dancelor_common_model.Kind.Base.Filter.predicate] [@@deriving qcheck]

      let shrink = function
        | Is _ -> Iter.empty

      type t = [%import: Dancelor_common_model.Kind.Base.Filter.t
                       [@with
                         Dancelor_common_model__.Formula.t := Formula.t;
        ]] [@@deriving qcheck]

      let shrink' = Formula.shrink shrink
    end
  end

  module Version = struct
    type t = [%import: Dancelor_common_model.Kind.Version.t]

    let gen = Gen.(pair nat Base.gen)
    let shrink _ = Iter.empty

    module Filter = struct
      type predicate = [%import: Dancelor_common_model.Kind.Version.Filter.predicate
                               [@with
                                 Dancelor_common_model__.KindBase.Filter.t := Base.Filter.t;
        ]] [@@deriving qcheck]

      let shrink =
        let open Iter in
        let open Dancelor_common_model.Kind.Version.Filter in
        function
        | Is k -> map is (shrink k)
        | BarsEq n -> map barsEq (Shrink.int n)
        | BarsNe n -> map barsNe (Shrink.int n)
        | BarsGt n -> map barsGt (Shrink.int n)
        | BarsGe n -> map barsGe (Shrink.int n)
        | BarsLt n -> map barsLt (Shrink.int n)
        | BarsLe n -> map barsLe (Shrink.int n)
        | Base bf -> map base (Base.Filter.shrink' bf)

      type t = [%import: Dancelor_common_model.Kind.Version.Filter.t
                       [@with
                         Dancelor_common_model__.Formula.t := Formula.t;
        ]] [@@deriving qcheck]

      let shrink' = Formula.shrink shrink
    end
  end

  module Dance = struct
    type t = [%import: Dancelor_common_model.Kind.Dance.t]

    let gen =
      let open Gen in
      let open Dancelor_common_model.Kind.Dance in
      sized @@ fix @@ fun self -> function
      | 0 -> version <$> Version.gen
      | n -> oneof [
          (add <$> self (n/2) <*> self (n/2));
          (mul <$> nat <*> self (n-1));
        ]

    let rec shrink =
      let open Iter in
      let open Dancelor_common_model.Kind.Dance in
      function
      | Version kv -> map version (Version.shrink kv)
      | Add (k1, k2) -> return k1 <+> return k2
                        <+> map (Fun.flip add k2) (shrink k1)
                        <+> map (add k1) (shrink k2);
      | Mul (n, k) -> return k <+> map (mul n) (shrink k)

    module Filter = struct
      type predicate = [%import: Dancelor_common_model.Kind.Dance.Filter.predicate
                               [@with
                                 Dancelor_common_model__.KindVersion.Filter.t := Version.Filter.t;
        ]] [@@deriving qcheck]

      let shrink =
        let open Iter in
        let open Dancelor_common_model.Kind.Dance.Filter in
        function
        | Simple -> empty
        | Is k -> return Simple <+> map is (shrink k)
        | Version vf -> return Simple <+> map version (Version.Filter.shrink' vf)

      type t = [%import: Dancelor_common_model.Kind.Dance.Filter.t
                       [@with
                         Dancelor_common_model__.Formula.t := Formula.t;
        ]] [@@deriving qcheck]

      let shrink' = Formula.shrink shrink
    end
  end
end

module Person = struct
  type t = [%import: Dancelor_common_model.PersonCore.t]

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
      generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen = Gen.pure (Obj.magic 0)

  module Filter = struct
    type predicate = [%import: Dancelor_common_model.PersonCore.Filter.predicate
                             [@with
                               Nes.Slug.t := Slug.t;
      ]] [@@deriving qcheck]

    (* FIXME: QCheck2 does this automatically. *)
    let shrink =
      let open Iter in
      let open Dancelor_common_model.PersonCore.Filter in
      function
      | Name string -> map name (Shrink.string string)
      | Is slug -> return (Name "a") <+> map is (Slug.shrink slug)
      | NameMatches string -> return (Name "a") <+> map nameMatches (Shrink.string string)

    type t = [%import: Dancelor_common_model.PersonCore.Filter.t
                     [@with
                       Dancelor_common_model__.Formula.t := Formula.t;
      ]] [@@deriving qcheck]

    let shrink' = Formula.shrink shrink
  end
end

module Dance = struct
  type t = [%import: Dancelor_common_model.DanceCore.t]

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
      generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen = Gen.pure (Obj.magic 0)

  module Filter = struct
    type predicate = [%import: Dancelor_common_model.DanceCore.Filter.predicate
                             [@with
                               Nes.Slug.t := Slug.t;
                               Dancelor_common_model__.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
                               Dancelor_common_model__.PersonCore.Filter.t := Person.Filter.t;
      ]] [@@deriving qcheck]

    (* FIXME: QCheck2 does this automatically. *)
    let shrink =
      let open Iter in
      let open Dancelor_common_model.DanceCore.Filter in
      function
      | Name string -> map name (Shrink.string string)
      | Is slug -> return (Name "a") <+> map is (Slug.shrink slug)
      | NameMatches string -> return (Name "a") <+> map nameMatches (Shrink.string string)
      | Deviser person -> return (Name "a") <+> map deviser (Person.Filter.shrink' person)
      | Kind k -> return (Name "a") <+> map kind (Kind.Dance.Filter.shrink' k)

    type t = [%import: Dancelor_common_model.DanceCore.Filter.t
                     [@with
                       Dancelor_common_model__.Formula.t := Formula.t
      ]] [@@deriving qcheck]

    let shrink' = Formula.shrink shrink
  end
end

module Tune = struct
  type t = [%import: Dancelor_common_model.TuneCore.t]

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
      generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen = Gen.pure (Obj.magic 0)

  module Filter = struct
    type predicate = [%import: Dancelor_common_model.TuneCore.Filter.predicate
                             [@with
                               Nes.Slug.t := Slug.t;
                               Dancelor_common_model__.Kind.Base.Filter.t := Kind.Base.Filter.t;
                               Dancelor_common_model__.PersonCore.Filter.t := Person.Filter.t;
                               Dancelor_common_model__.DanceCore.Filter.t := Dance.Filter.t;
      ]] [@@deriving qcheck]

    (* FIXME: QCheck2 does this automatically. *)
    let shrink =
      let open Iter in
      let open Dancelor_common_model.TuneCore.Filter in
      function
      | Name string -> map name (Shrink.string string)
      | Is slug -> return (Name "a") <+> map is (Slug.shrink slug)
      | NameMatches string -> return (Name "a") <+> map nameMatches (Shrink.string string)
      | Author person -> return (Name "a") <+> map author (Person.Filter.shrink' person)
      | Kind kf -> return (Name "a") <+> map kind (Kind.Base.Filter.shrink' kf)
      | ExistsDance df -> return (Name "a") <+> map existsDance (Dance.Filter.shrink' df)

    type t = [%import: Dancelor_common_model.TuneCore.Filter.t
                     [@with
                       Dancelor_common_model__.Formula.t := Formula.t;
      ]] [@@deriving qcheck]

    let shrink' = Formula.shrink shrink
  end
end

module Version = struct
  type t = [%import: Dancelor_common_model.VersionCore.t]

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
      generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen = Gen.pure (Obj.magic 0)

  module Filter = struct
    type predicate = [%import: Dancelor_common_model.VersionCore.Filter.predicate
                             [@with
                               Nes.Slug.t := Slug.t;
                               Dancelor_common_model__.Music.key := Music.key;
                               Dancelor_common_model__.Kind.Base.Filter.t := Kind.Base.Filter.t;
                               Dancelor_common_model__.Kind.Version.Filter.t := Kind.Version.Filter.t;
                               Dancelor_common_model__.PersonCore.Filter.t := Person.Filter.t;
                               Dancelor_common_model__.DanceCore.Filter.t := Dance.Filter.t;
                               Dancelor_common_model__.TuneCore.Filter.t := Tune.Filter.t;
      ]] [@@deriving qcheck]

    (* FIXME: QCheck2 does this automatically. *)
    let shrink =
      let open Iter in
      let open Dancelor_common_model.VersionCore.Filter in
      function
      | Broken -> empty
      | Is slug -> return Broken <+> map is (Slug.shrink slug)
      | Tune tf -> return Broken <+> map tune (Tune.Filter.shrink' tf)
      | Kind kf -> return Broken <+> map kind (Kind.Version.Filter.shrink' kf)
      | Key _ -> return Broken

    type t = [%import: Dancelor_common_model.VersionCore.Filter.t
                     [@with
                       Dancelor_common_model__.Formula.t := Formula.t;
      ]] [@@deriving qcheck]

    let shrink' = Formula.shrink shrink
  end
end

module Set = struct
  type t = [%import: Dancelor_common_model.SetCore.t]

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
      generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen = Gen.pure (Obj.magic 0)

  module Filter = struct
    type predicate = [%import: Dancelor_common_model.SetCore.Filter.predicate
                             [@with
                               Nes.Slug.t := Slug.t;
                               Dancelor_common_model__.Music.key := Music.key;
                               Dancelor_common_model__.Kind.Base.Filter.t := Kind.Base.Filter.t;
                               Dancelor_common_model__.Kind.Version.Filter.t := Kind.Version.Filter.t;
                               Dancelor_common_model__.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
                               Dancelor_common_model__.PersonCore.Filter.t := Person.Filter.t;
                               Dancelor_common_model__.DanceCore.Filter.t := Dance.Filter.t;
                               Dancelor_common_model__.TuneCore.Filter.t := Tune.Filter.t;
                               Dancelor_common_model__.VersionCore.Filter.t := Version.Filter.t;
      ]] [@@deriving qcheck]

    (* FIXME: QCheck2 does this automatically. *)
    let shrink =
      let open Iter in
      let open Dancelor_common_model.SetCore.Filter in
      function
      | Name string -> map name (Shrink.string string)
      | Is slug -> return (Name "a") <+> map is (Slug.shrink slug)
      | NameMatches string -> return (Name "a") <+> map nameMatches (Shrink.string string)
      | Deviser pf -> return (Name "a") <+> map deviser (Person.Filter.shrink' pf)
      | ExistsVersion vf -> return (Name "a") <+> map existsVersion (Version.Filter.shrink' vf)
      | Kind kf -> return (Name "a") <+> map kind (Kind.Dance.Filter.shrink' kf)

    type t = [%import: Dancelor_common_model.SetCore.Filter.t
                     [@with
                       Dancelor_common_model__.Formula.t := Formula.t;
      ]] [@@deriving qcheck]

    let shrink' = Formula.shrink shrink
  end
end

module Book = struct
  type t = Dancelor_common_model.BookCore.t

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
      generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen = Gen.pure (Obj.magic 0)

  module Filter = struct
    type predicate = [%import: Dancelor_common_model.BookCore.Filter.predicate
                             [@with
                               Nes.Slug.t := Slug.t;
                               Dancelor_common_model__.Music.key := Music.key;
                               Dancelor_common_model__.Kind.Base.Filter.t := Kind.Base.Filter.t;
                               Dancelor_common_model__.Kind.Version.Filter.t := Kind.Version.Filter.t;
                               Dancelor_common_model__.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
                               Dancelor_common_model__.PersonCore.Filter.t := Person.Filter.t;
                               Dancelor_common_model__.DanceCore.Filter.t := Dance.Filter.t;
                               Dancelor_common_model__.TuneCore.Filter.t := Tune.Filter.t;
                               Dancelor_common_model__.VersionCore.Filter.t := Version.Filter.t;
                               Dancelor_common_model__.SetCore.Filter.t := Set.Filter.t;
      ]] [@@deriving qcheck]

    (* FIXME: QCheck2 does this automatically. *)
    let shrink =
      let open Iter in
      let open Dancelor_common_model.BookCore.Filter in
      function
      | IsSource -> empty
      | Is slug -> return IsSource <+> map is (Slug.shrink slug)
      | Title string -> return IsSource <+> map title (Shrink.string string)
      | TitleMatches string -> return IsSource <+> map titleMatches (Shrink.string string)
      | Subtitle string -> return IsSource <+> map subtitle (Shrink.string string)
      | SubtitleMatches string -> return IsSource <+> map subtitleMatches (Shrink.string string)
      | ExistsVersion vf -> return IsSource <+> map existsVersion (Version.Filter.shrink' vf)
      | ExistsSet sf -> return IsSource <+> map existsSet (Set.Filter.shrink' sf)
      | ExistsInlineSet sf -> return IsSource <+> map existsInlineSet (Set.Filter.shrink' sf)
      | ExistsVersionDeep vf -> return IsSource <+> map existsVersionDeep (Version.Filter.shrink' vf)

    type t = [%import: Dancelor_common_model.BookCore.Filter.t
                     [@with
                       Dancelor_common_model__.Formula.t := Formula.t;
      ]] [@@deriving qcheck]

    let shrink' = Formula.shrink shrink
  end
end

module Any = struct
  module Type = struct
    type t = [%import: Dancelor_common_model.AnyCore.Type.t
                     [@with
                       Dancelor_common_model__.Formula.t := Formula.t;
      ]] [@@deriving qcheck]

    let shrink = let open Iter in function
        | Person -> empty
        | _ -> return Person
  end

  module Filter = struct
    type predicate = [%import: Dancelor_common_model.AnyCore.Filter.predicate
                             [@with
                               Nes.Slug.t := Slug.t;
                               Dancelor_common_model__.Music.key := Music.key;
                               Dancelor_common_model__.Kind.Base.Filter.t := Kind.Base.Filter.t;
                               Dancelor_common_model__.Kind.Version.Filter.t := Kind.Version.Filter.t;
                               Dancelor_common_model__.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
                               Dancelor_common_model__.PersonCore.Filter.t := Person.Filter.t;
                               Dancelor_common_model__.DanceCore.Filter.t := Dance.Filter.t;
                               Dancelor_common_model__.TuneCore.Filter.t := Tune.Filter.t;
                               Dancelor_common_model__.VersionCore.Filter.t := Version.Filter.t;
                               Dancelor_common_model__.SetCore.Filter.t := Set.Filter.t;
                               Dancelor_common_model__.BookCore.Filter.t := Book.Filter.t;
      ]] [@@deriving qcheck]

    (* FIXME: QCheck2 does this automatically. *)
    let shrink =
      let open Iter in
      let open Dancelor_common_model.AnyCore.Filter in
      function
      | Type t -> map type_ (Type.shrink t)
      | Person p -> (return (Type Person)) <+> map person (Person.Filter.shrink' p)
      | Dance d -> (return (Type Person)) <+> map dance (Dance.Filter.shrink' d)
      | Book b -> (return (Type Person)) <+> map book (Book.Filter.shrink' b)
      | Set s -> (return (Type Person)) <+> map set (Set.Filter.shrink' s)
      | Tune t -> (return (Type Person)) <+> map tune (Tune.Filter.shrink' t)
      | Version v -> (return (Type Person)) <+> map version (Version.Filter.shrink' v)

    type t = [%import: Dancelor_common_model.AnyCore.Filter.t
                     [@with
                       Dancelor_common_model__.Formula.t := Formula.t;
      ]] [@@deriving qcheck]

    let shrink' = Formula.shrink shrink
  end
end
