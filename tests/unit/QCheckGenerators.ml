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

module Source = struct
  type t = [%import: Common.ModelBuilder.Core.Source.t]

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
      generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)

  module Filter = struct
    type predicate = [%import: Common.ModelBuilder.Filter.Source.predicate [@with Nes.Slug.t := Slug.t;
      (* Core *)
      Common__ModelBuilder__.Core.Source.t := t;
      ]
    ]
    [@@deriving qcheck2]

    type t = [%import: Common.ModelBuilder.Filter.Source.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end
end

module Person = struct
  type t = [%import: Common.ModelBuilder.Core.Person.t]

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
      generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)

  module Filter = struct
    type predicate = [%import: Common.ModelBuilder.Filter.Person.predicate [@with Nes.Slug.t := Slug.t;
      (* Core *)
      Common__ModelBuilder__.Core.Person.t := t;
      ]
    ]
    [@@deriving qcheck2]

    type t = [%import: Common.ModelBuilder.Filter.Person.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end
end

module Dance = struct
  type t = [%import: Common.ModelBuilder.Core.Dance.t]

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
      generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)

  module Filter = struct
    type predicate = [%import: Common.ModelBuilder.Filter.Dance.predicate [@with Nes.Slug.t := Slug.t;
      (* Core *)
      Common__ModelBuilder__.Core.Dance.t := t;
      (* Filter *)
      Common.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
      Common__ModelBuilder__Filter.Person.t := Person.Filter.t;
      ]
    ]
    [@@deriving qcheck2]

    type t = [%import: Common.ModelBuilder.Filter.Dance.t [@with Common.Formula.t := Formula.t]
    ]
    [@@deriving qcheck2]
  end
end

module Tune = struct
  type t = [%import: Common.ModelBuilder.Core.Tune.t]

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
      generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)

  module Filter = struct
    type predicate = [%import: Common.ModelBuilder.Filter.Tune.predicate [@with Nes.Slug.t := Slug.t;
      (* Core *)
      Common__ModelBuilder__.Core.Tune.t := t;
      Common__ModelBuilder__.Core.Dance.t := Dance.t;
      (* Filter *)
      Common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Common__ModelBuilder__Filter.Person.t := Person.Filter.t;
      Common__ModelBuilder__Filter.Dance.t := Dance.Filter.t;
      ]
    ]
    [@@deriving qcheck2]

    type t = [%import: Common.ModelBuilder.Filter.Tune.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end
end

module Version = struct
  type t = [%import: Common.ModelBuilder.Core.Version.t]

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
      generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)

  module Filter = struct
    type predicate = [%import: Common.ModelBuilder.Filter.Version.predicate [@with Nes.Slug.t := Slug.t;
      Common.Music.key := Music.key;
      (* Core *)
      Common__ModelBuilder__.Core.Version.t := t;
      (* Filter *)
      Common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Common.Kind.Version.Filter.t := Kind.Version.Filter.t;
      Common__ModelBuilder__Filter.Source.t := Source.Filter.t;
      Common__ModelBuilder__Filter.Person.t := Person.Filter.t;
      Common__ModelBuilder__Filter.Dance.t := Dance.Filter.t;
      Common__ModelBuilder__Filter.Tune.t := Tune.Filter.t;
      ]
    ]
    [@@deriving qcheck2]

    type t = [%import: Common.ModelBuilder.Filter.Version.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end
end

module Set = struct
  type t = [%import: Common.ModelBuilder.Core.Set.t]

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
      generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)

  module Filter = struct
    type predicate = [%import: Common.ModelBuilder.Filter.Set.predicate [@with Nes.Slug.t := Slug.t;
      Common.Music.key := Music.key;
      (* Core *)
      Common__ModelBuilder__.Core.Set.t := t;
      (* Filter *)
      Common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Common.Kind.Version.Filter.t := Kind.Version.Filter.t;
      Common.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
      Common__ModelBuilder__Filter.Person.t := Person.Filter.t;
      Common__ModelBuilder__Filter.Version.t := Version.Filter.t;
      ]
    ]
    [@@deriving qcheck2]

    type t = [%import: Common.ModelBuilder.Filter.Set.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end
end

module Book = struct
  type t = Common.ModelBuilder.Core.Book.t

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
      generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)

  module Filter = struct
    type predicate = [%import: Common.ModelBuilder.Filter.Book.predicate [@with Nes.Slug.t := Slug.t;
      Common.Music.key := Music.key;
      (* Core *)
      Common__ModelBuilder__.Core.Book.t := t;
      (* Filter *)
      Common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Common.Kind.Version.Filter.t := Kind.Version.Filter.t;
      Common.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
      Common__ModelBuilder__Filter.Version.t := Version.Filter.t;
      Common__ModelBuilder__Filter.Set.t := Set.Filter.t;
      ]
    ]
    [@@deriving qcheck2]

    type t = [%import: Common.ModelBuilder.Filter.Book.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end
end

module Any = struct
  module Type = struct
    type t = [%import: Common.ModelBuilder.Core.Any.Type.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Filter = struct
    type predicate = [%import: Common.ModelBuilder.Filter.Any.predicate [@with Nes.Slug.t := Slug.t;
      Common.Music.key := Music.key;
      (* Core *)
      Common__ModelBuilder__.Core.Any.Type.t := Type.t;
      (* Filter *)
      Common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Common.Kind.Version.Filter.t := Kind.Version.Filter.t;
      Common.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
      Common__ModelBuilder__Filter.Source.t := Source.Filter.t;
      Common__ModelBuilder__Filter.Person.t := Person.Filter.t;
      Common__ModelBuilder__Filter.Dance.t := Dance.Filter.t;
      Common__ModelBuilder__Filter.Book.t := Book.Filter.t;
      Common__ModelBuilder__Filter.Set.t := Set.Filter.t;
      Common__ModelBuilder__Filter.Tune.t := Tune.Filter.t;
      Common__ModelBuilder__Filter.Version.t := Version.Filter.t;
      ]
    ]
    [@@deriving qcheck2]

    type t = [%import: Common.ModelBuilder.Filter.Any.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end
end
