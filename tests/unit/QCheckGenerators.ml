open QCheck2
module Model = Dancelor_common_model

module Slug = struct
  type 'any t = [%import: 'any NesSlug.t]

  let gen _ =
    Gen.map
      NesSlug.unsafe_of_string
      Gen.(string_size ~gen: (char_range 'a' 'z') (int_range 1 10))
end

module Music = struct
  (* FIXME: Should be able to go in the [@@deriving] block. *)

  type note = [%import: Dancelor_common_model.Music.note] [@@deriving qcheck2]
  type alteration = [%import: Dancelor_common_model.Music.alteration] [@@deriving qcheck2]

  type octave = [%import: Dancelor_common_model.Music.octave]
  let gen_octave = Gen.int_range (-8) 8

  type pitch = [%import: Dancelor_common_model.Music.pitch] [@@deriving qcheck2]
  type mode = [%import: Dancelor_common_model.Music.mode] [@@deriving qcheck2]
  type key = [%import: Dancelor_common_model.Music.key] [@@deriving qcheck2]
  type clef = [%import: Dancelor_common_model.Music.clef] [@@deriving qcheck2]
end

module Formula = struct
  type 'p t = [%import: 'p Dancelor_common_model.Formula.t] [@@deriving qcheck2]

  (* Formulas can grow very quickly. We therefore limit the size of formulas
     drastically in our generator. *)
  let gen gen_p = gen_sized gen_p 10
end

module TextFormula = struct
  type predicate = [%import: Dancelor_common_model.TextFormula.predicate]
  and t = [%import: Dancelor_common_model.TextFormula.t]

  let gen_predicate_name =
    let open Gen in
    fix
      (fun self () ->
        let* name = string_size ~gen: (char_range 'a' 'z') (int_range 1 10) in
        if name = "or" || name = "and" || name = "not" then
          self ()
        else
          pure name
      )
      ()

  let gen_predicate =
    let open Gen in
    fix
      (fun self () ->
        oneof
          [
            (Dancelor_common_model.TextFormula.raw <$> string_printable);
            (Dancelor_common_model.TextFormula.nullary <$> gen_predicate_name);
            (Dancelor_common_model.TextFormula.unary <$> gen_predicate_name <*> Formula.gen (self ()));
          ]
      )
      ()

  let gen = Formula.gen gen_predicate
end

module Kind = struct
  module Base = struct
    type t = [%import: Dancelor_common_model.Kind.Base.t] [@@deriving qcheck2]

    module Filter = struct
      type predicate = [%import: Dancelor_common_model.Kind.Base.Filter.predicate] [@@deriving qcheck2]

      type t = [%import: Dancelor_common_model.Kind.Base.Filter.t [@with Dancelor_common_model__.Formula.t := Formula.t;]
      ]
      [@@deriving qcheck2]
    end
  end

  module Version = struct
    type t = [%import: Dancelor_common_model.Kind.Version.t]

    let gen = Gen.(pair nat Base.gen)

    module Filter = struct
      type predicate = [%import: Dancelor_common_model.Kind.Version.Filter.predicate [@with Dancelor_common_model__.KindBase.Filter.t := Base.Filter.t;]
      ]
      [@@deriving qcheck2]

      type t = [%import: Dancelor_common_model.Kind.Version.Filter.t [@with Dancelor_common_model__.Formula.t := Formula.t;]
      ]
      [@@deriving qcheck2]
    end
  end

  module Dance = struct
    type t = [%import: Dancelor_common_model.Kind.Dance.t]

    let gen =
      let open Gen in
      let open Dancelor_common_model.Kind.Dance in
      sized @@
      fix @@ fun self ->
      function
        | 0 -> version <$> Version.gen
        | n ->
          oneof
            [
              (add <$> self (n / 2) <*> self (n / 2));
              (mul <$> nat <*> self (n - 1));
            ]

    module Filter = struct
      type predicate = [%import: Dancelor_common_model.Kind.Dance.Filter.predicate [@with Dancelor_common_model__.KindVersion.Filter.t := Version.Filter.t;]
      ]
      [@@deriving qcheck2]

      type t = [%import: Dancelor_common_model.Kind.Dance.Filter.t [@with Dancelor_common_model__.Formula.t := Formula.t;]
      ]
      [@@deriving qcheck2]
    end
  end
end

module Person = struct
  type t = [%import: Dancelor_common_model.PersonCore.t]

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
      generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)

  module Filter = struct
    type predicate = [%import: Dancelor_common_model.PersonCore.Filter.predicate [@with Nes.Slug.t := Slug.t;]
    ]
    [@@deriving qcheck2]

    type t = [%import: Dancelor_common_model.PersonCore.Filter.t [@with Dancelor_common_model__.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end
end

module Dance = struct
  type t = [%import: Dancelor_common_model.DanceCore.t]

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
      generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)

  module Filter = struct
    type predicate = [%import: Dancelor_common_model.DanceCore.Filter.predicate [@with Nes.Slug.t := Slug.t;
      Dancelor_common_model__.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
      Dancelor_common_model__.PersonCore.Filter.t := Person.Filter.t;
      ]
    ]
    [@@deriving qcheck2]

    type t = [%import: Dancelor_common_model.DanceCore.Filter.t [@with Dancelor_common_model__.Formula.t := Formula.t]
    ]
    [@@deriving qcheck2]
  end
end

module Tune = struct
  type t = [%import: Dancelor_common_model.TuneCore.t]

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
      generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)

  module Filter = struct
    type predicate = [%import: Dancelor_common_model.TuneCore.Filter.predicate [@with Nes.Slug.t := Slug.t;
      Dancelor_common_model__.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Dancelor_common_model__.PersonCore.Filter.t := Person.Filter.t;
      Dancelor_common_model__.DanceCore.Filter.t := Dance.Filter.t;
      ]
    ]
    [@@deriving qcheck2]

    type t = [%import: Dancelor_common_model.TuneCore.Filter.t [@with Dancelor_common_model__.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end
end

module Version = struct
  type t = [%import: Dancelor_common_model.VersionCore.t]

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
      generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)

  module Filter = struct
    type predicate = [%import: Dancelor_common_model.VersionCore.Filter.predicate [@with Nes.Slug.t := Slug.t;
      Dancelor_common_model__.Music.key := Music.key;
      Dancelor_common_model__.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Dancelor_common_model__.Kind.Version.Filter.t := Kind.Version.Filter.t;
      Dancelor_common_model__.PersonCore.Filter.t := Person.Filter.t;
      Dancelor_common_model__.DanceCore.Filter.t := Dance.Filter.t;
      Dancelor_common_model__.TuneCore.Filter.t := Tune.Filter.t;
      ]
    ]
    [@@deriving qcheck2]

    type t = [%import: Dancelor_common_model.VersionCore.Filter.t [@with Dancelor_common_model__.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end
end

module Set = struct
  type t = [%import: Dancelor_common_model.SetCore.t]

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
      generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)

  module Filter = struct
    type predicate = [%import: Dancelor_common_model.SetCore.Filter.predicate [@with Nes.Slug.t := Slug.t;
      Dancelor_common_model__.Music.key := Music.key;
      Dancelor_common_model__.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Dancelor_common_model__.Kind.Version.Filter.t := Kind.Version.Filter.t;
      Dancelor_common_model__.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
      Dancelor_common_model__.PersonCore.Filter.t := Person.Filter.t;
      Dancelor_common_model__.DanceCore.Filter.t := Dance.Filter.t;
      Dancelor_common_model__.TuneCore.Filter.t := Tune.Filter.t;
      Dancelor_common_model__.VersionCore.Filter.t := Version.Filter.t;
      ]
    ]
    [@@deriving qcheck2]

    type t = [%import: Dancelor_common_model.SetCore.Filter.t [@with Dancelor_common_model__.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end
end

module Book = struct
  type t = Dancelor_common_model.BookCore.t

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
      generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)

  module Filter = struct
    type predicate = [%import: Dancelor_common_model.BookCore.Filter.predicate [@with Nes.Slug.t := Slug.t;
      Dancelor_common_model__.Music.key := Music.key;
      Dancelor_common_model__.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Dancelor_common_model__.Kind.Version.Filter.t := Kind.Version.Filter.t;
      Dancelor_common_model__.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
      Dancelor_common_model__.PersonCore.Filter.t := Person.Filter.t;
      Dancelor_common_model__.DanceCore.Filter.t := Dance.Filter.t;
      Dancelor_common_model__.TuneCore.Filter.t := Tune.Filter.t;
      Dancelor_common_model__.VersionCore.Filter.t := Version.Filter.t;
      Dancelor_common_model__.SetCore.Filter.t := Set.Filter.t;
      ]
    ]
    [@@deriving qcheck2]

    type t = [%import: Dancelor_common_model.BookCore.Filter.t [@with Dancelor_common_model__.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end
end

module Any = struct
  module Type = struct
    type t = [%import: Dancelor_common_model.AnyCore.Type.t [@with Dancelor_common_model__.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Filter = struct
    type predicate = [%import: Dancelor_common_model.AnyCore.Filter.predicate [@with Nes.Slug.t := Slug.t;
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
      ]
    ]
    [@@deriving qcheck2]

    type t = [%import: Dancelor_common_model.AnyCore.Filter.t [@with Dancelor_common_model__.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end
end
