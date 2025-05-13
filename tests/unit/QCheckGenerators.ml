open QCheck2

module Slug = struct
  type 'any t = [%import: 'any NesSlug.t]

  let gen _ =
    Gen.map
      NesSlug.unsafe_of_string
      Gen.(string_size ~gen: (char_range 'a' 'z') (int_range 1 10))
end

module Music = struct
  (* FIXME: Should be able to go in the [@@deriving] block. *)

  type note = [%import: Common.Music.note] [@@deriving qcheck2]
  type alteration = [%import: Common.Music.alteration] [@@deriving qcheck2]

  type octave = [%import: Common.Music.octave]
  let gen_octave = Gen.int_range (-8) 8

  type pitch = [%import: Common.Music.pitch] [@@deriving qcheck2]
  type mode = [%import: Common.Music.mode] [@@deriving qcheck2]
  type key = [%import: Common.Music.key] [@@deriving qcheck2]
  type clef = [%import: Common.Music.clef] [@@deriving qcheck2]
end

module Formula = struct
  type 'p t = [%import: 'p Common.Formula.t] [@@deriving qcheck2]

  (* Formulas can grow very quickly. We therefore limit the size of formulas
     drastically in our generator. *)
  let gen gen_p = gen_sized gen_p 10
end

module TextFormula = struct
  type predicate = [%import: Common.TextFormula.predicate]
  and t = [%import: Common.TextFormula.t]

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
        oneof [
          (Common.TextFormula.raw <$> string_printable);
          (Common.TextFormula.nullary <$> gen_predicate_name);
          (Common.TextFormula.unary <$> gen_predicate_name <*> Formula.gen (self ()));
        ]
      )
      ()

  let gen = Formula.gen gen_predicate
end

module Kind = struct
  module Base = struct
    type t = [%import: Common.Kind.Base.t] [@@deriving qcheck2]

    module Filter = struct
      type predicate = [%import: Common.Kind.Base.Filter.predicate] [@@deriving qcheck2]

      type t = [%import: Common.Kind.Base.Filter.t [@with Common.Formula.t := Formula.t;]
      ]
      [@@deriving qcheck2]
    end
  end

  module Version = struct
    type t = [%import: Common.Kind.Version.t]

    let gen = Gen.(pair nat Base.gen)

    module Filter = struct
      type predicate = [%import: Common.Kind.Version.Filter.predicate [@with Common.KindBase.Filter.t := Base.Filter.t;]
      ]
      [@@deriving qcheck2]

      type t = [%import: Common.Kind.Version.Filter.t [@with Common.Formula.t := Formula.t;]
      ]
      [@@deriving qcheck2]
    end
  end

  module Dance = struct
    type t = [%import: Common.Kind.Dance.t]

    let gen =
      let open Gen in
      let open Common.Kind.Dance in
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
      type predicate = [%import: Common.Kind.Dance.Filter.predicate [@with Common.KindVersion.Filter.t := Version.Filter.t;]
      ]
      [@@deriving qcheck2]

      type t = [%import: Common.Kind.Dance.Filter.t [@with Common.Formula.t := Formula.t;]
      ]
      [@@deriving qcheck2]
    end
  end
end

module Model = struct
  (* The following are dirty tricks, necessary to convince [ppx_deriving_qcheck]
     that it can generate a [t Slug.t]. These “models” are only to be used in a
     context where we use their slugs; there, it is fine since [Slug.gen]
     ignores its first argument. *)

  module Source = struct
    type t = [%import: Common.ModelBuilder.Core.Source.t]
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Person = struct
    type t = [%import: Common.ModelBuilder.Core.Person.t]
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Dance = struct
    type t = [%import: Common.ModelBuilder.Core.Dance.t]
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Tune = struct
    type t = [%import: Common.ModelBuilder.Core.Tune.t]
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Version = struct
    type t = [%import: Common.ModelBuilder.Core.Version.t]
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Set = struct
    type t = [%import: Common.ModelBuilder.Core.Set.t]
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Book = struct
    type t = Common.ModelBuilder.Core.Book.t
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Any = struct
    module Type = struct
      type t = [%import: Common.ModelBuilder.Core.Any.Type.t [@with Common.Formula.t := Formula.t;]
      ]
      [@@deriving qcheck2]
    end
  end
end

module Filter = struct
  module Source = struct
    type predicate = [%import: Common.FilterBuilder.Core.Source.predicate [@with Nes.Slug.t := Slug.t;
      Common.ModelBuilder.Core.Source.t := Model.Source.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Common.FilterBuilder.Core.Source.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Person = struct
    type predicate = [%import: Common.FilterBuilder.Core.Person.predicate [@with Nes.Slug.t := Slug.t;
      Common.ModelBuilder.Core.Person.t := Model.Person.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Common.FilterBuilder.Core.Person.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Dance = struct
    type predicate = [%import: Common.FilterBuilder.Core.Dance.predicate [@with Nes.Slug.t := Slug.t;
      Common.ModelBuilder.Core.Dance.t := Model.Dance.t;
      Common.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
      Common__FilterBuilder__Core.Person.t := Person.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Common.FilterBuilder.Core.Dance.t [@with Common.Formula.t := Formula.t]
    ]
    [@@deriving qcheck2]
  end

  module Tune = struct
    type predicate = [%import: Common.FilterBuilder.Core.Tune.predicate [@with Nes.Slug.t := Slug.t;
      Common.ModelBuilder.Core.Tune.t := Model.Tune.t;
      Common.ModelBuilder.Core.Dance.t := Model.Dance.t;
      Common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Common__FilterBuilder__Core.Person.t := Person.t;
      Common__FilterBuilder__Core.Dance.t := Dance.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Common.FilterBuilder.Core.Tune.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Version = struct
    type predicate = [%import: Common.FilterBuilder.Core.Version.predicate [@with Nes.Slug.t := Slug.t;
      Common.Music.key := Music.key;
      Common.ModelBuilder.Core.Version.t := Model.Version.t;
      Common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Common.Kind.Version.Filter.t := Kind.Version.Filter.t;
      Common__FilterBuilder__Core.Source.t := Source.t;
      Common__FilterBuilder__Core.Person.t := Person.t;
      Common__FilterBuilder__Core.Dance.t := Dance.t;
      Common__FilterBuilder__Core.Tune.t := Tune.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Common.FilterBuilder.Core.Version.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Set = struct
    type predicate = [%import: Common.FilterBuilder.Core.Set.predicate [@with Nes.Slug.t := Slug.t;
      Common.Music.key := Music.key;
      Common.ModelBuilder.Core.Set.t := Model.Set.t;
      Common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Common.Kind.Version.Filter.t := Kind.Version.Filter.t;
      Common.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
      Common__FilterBuilder__Core.Person.t := Person.t;
      Common__FilterBuilder__Core.Version.t := Version.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Common.FilterBuilder.Core.Set.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Book = struct
    type predicate = [%import: Common.FilterBuilder.Core.Book.predicate [@with Nes.Slug.t := Slug.t;
      Common.Music.key := Music.key;
      Common.ModelBuilder.Core.Book.t := Model.Book.t;
      Common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Common.Kind.Version.Filter.t := Kind.Version.Filter.t;
      Common.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
      Common__FilterBuilder__Core.Version.t := Version.t;
      Common__FilterBuilder__Core.Set.t := Set.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Common.FilterBuilder.Core.Book.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Any = struct
    type predicate = [%import: Common.FilterBuilder.Core.Any.predicate [@with Nes.Slug.t := Slug.t;
      Common.Music.key := Music.key;
      Common.ModelBuilder.Core.Any.Type.t := Model.Any.Type.t;
      Common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Common.Kind.Version.Filter.t := Kind.Version.Filter.t;
      Common.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
      Common__FilterBuilder__Core.Source.t := Source.t;
      Common__FilterBuilder__Core.Person.t := Person.t;
      Common__FilterBuilder__Core.Dance.t := Dance.t;
      Common__FilterBuilder__Core.Book.t := Book.t;
      Common__FilterBuilder__Core.Set.t := Set.t;
      Common__FilterBuilder__Core.Tune.t := Tune.t;
      Common__FilterBuilder__Core.Version.t := Version.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Common.FilterBuilder.Core.Any.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end
end
