open QCheck2

module Id = struct
  type 'any t = [%import: 'any Common.Entry.Id.t]

  let gen _ =
    let open Gen in
    (* generate 11 digits in base 36 *)
    flatten_list (List.init 11 (fun _ -> int_range 0 35)) >>= fun digits ->
    (* add the checksum digit in front *)
    let digits = ((36 - ((List.fold_left (+) 0 digits) mod 36)) mod 36) :: digits in
    (* turn the digits into alphanumerals *)
    let alphanumerals = List.map (fun n -> Char.chr (if n < 10 then Char.code '0' + n else Char.code 'a' + n - 10)) digits in
    (* make a string of the form 0000-0000-0000 *)
    let str = Bytes.make 14 '-' in
    for i = 0 to 3 do Bytes.set str i (List.nth alphanumerals i) done;
    for i = 5 to 8 do Bytes.set str i (List.nth alphanumerals (i - 1)) done;
    for i = 10 to 13 do Bytes.set str i (List.nth alphanumerals (i - 2)) done;
    let str = Bytes.unsafe_to_string str in
    Format.eprintf "===> %s@." str;
    (* convert to and id (which will check it), and we're done! *)
    pure @@ Option.get @@ Common.Entry.Id.of_string str
end

module Formula = struct
  type 'p t = [%import: 'p Common.Formula.t] [@@deriving qcheck2]

  (* Formulas can grow very quickly. We therefore limit the size of formulas
     drastically in our generator. *)
  let gen gen_p = gen_sized gen_p 10
end

module Text_formula = struct
  type predicate = [%import: Common.Text_formula.predicate]
  and t = [%import: Common.Text_formula.t]

  let gen_predicate_name =
    let open Gen in
    fix
      (fun self () ->
        let* name = string_size ~gen: (char_range 'a' 'z') (int_range 1 10) in
        if name = "or" || name = "and" || name = "not" || name = "true" || name = "false" then
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
          (Common.Text_formula.raw <$> string_printable);
          (Common.Text_formula.nullary <$> gen_predicate_name);
          (Common.Text_formula.unary <$> gen_predicate_name <*> Formula.gen (self ()));
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
      type predicate = [%import: Common.Kind.Version.Filter.predicate [@with Common.Kind_base.Filter.t := Base.Filter.t;]
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
      type predicate = [%import: Common.Kind.Dance.Filter.predicate [@with Common.Kind_version.Filter.t := Version.Filter.t;]
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
     that it can generate an [t Id.t]. These “models” are only to be used in a
     context where we use their id; there, it is fine since [Id.gen] ignores its
     first argument. *)

  module Source = struct
    type t = Common.Model_builder.Core.Source.t
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Person = struct
    type t = Common.Model_builder.Core.Person.t
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Dance = struct
    type t = Common.Model_builder.Core.Dance.t
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Tune = struct
    type t = Common.Model_builder.Core.Tune.t
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Version = struct
    type t = Common.Model_builder.Core.Version.t
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Set = struct
    type t = Common.Model_builder.Core.Set.t
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Book = struct
    type t = Common.Model_builder.Core.Book.t
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Any = struct
    module Type = struct
      type t = [%import: Common.Model_builder.Core.Any.Type.t [@with Common.Formula.t := Formula.t;]
      ]
      [@@deriving qcheck2]
    end
  end
end

module Filter = struct
  module Person = struct
    type predicate = [%import: Common.Filter_builder.Core.Person.predicate [@with Common.Entry.Id.t := Id.t;
      Common.Entry.id := Id.t;
      Common.Model_builder.Core.Person.t := Model.Person.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Common.Filter_builder.Core.Person.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Source = struct
    type predicate = [%import: Common.Filter_builder.Core.Source.predicate [@with Common.Entry.Id.t := Id.t;
      Common.Entry.id := Id.t;
      Common.Model_builder.Core.Source.t := Model.Source.t;
      Common__Filter_builder__Core.Person.t := Person.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Common.Filter_builder.Core.Source.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Dance = struct
    type predicate = [%import: Common.Filter_builder.Core.Dance.predicate [@with Common.Entry.Id.t := Id.t;
      Common.Entry.id := Id.t;
      Common.Model_builder.Core.Dance.t := Model.Dance.t;
      Common.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
      Common__Filter_builder__Core.Person.t := Person.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Common.Filter_builder.Core.Dance.t [@with Common.Formula.t := Formula.t]
    ]
    [@@deriving qcheck2]
  end

  module Tune = struct
    type predicate = [%import: Common.Filter_builder.Core.Tune.predicate [@with Common.Entry.Id.t := Id.t;
      Common.Model_builder.Core.Tune.t := Model.Tune.t;
      Common.Model_builder.Core.Dance.t := Model.Dance.t;
      Common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Common__Filter_builder__Core.Person.t := Person.t;
      Common__Filter_builder__Core.Dance.t := Dance.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Common.Filter_builder.Core.Tune.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Version = struct
    type predicate = [%import: Common.Filter_builder.Core.Version.predicate [@with Common.Entry.Id.t := Id.t;
      Common.Model_builder.Core.Version.t := Model.Version.t;
      Common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Common.Kind.Version.Filter.t := Kind.Version.Filter.t;
      Common__Filter_builder__Core.Source.t := Source.t;
      Common__Filter_builder__Core.Person.t := Person.t;
      Common__Filter_builder__Core.Dance.t := Dance.t;
      Common__Filter_builder__Core.Tune.t := Tune.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Common.Filter_builder.Core.Version.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Set = struct
    type predicate = [%import: Common.Filter_builder.Core.Set.predicate [@with Common.Entry.Id.t := Id.t;
      Common.Model_builder.Core.Set.t := Model.Set.t;
      Common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Common.Kind.Version.Filter.t := Kind.Version.Filter.t;
      Common.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
      Common__Filter_builder__Core.Person.t := Person.t;
      Common__Filter_builder__Core.Version.t := Version.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Common.Filter_builder.Core.Set.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Book = struct
    type predicate = [%import: Common.Filter_builder.Core.Book.predicate [@with Common.Entry.Id.t := Id.t;
      Common.Model_builder.Core.Book.t := Model.Book.t;
      Common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Common.Kind.Version.Filter.t := Kind.Version.Filter.t;
      Common.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
      Common__Filter_builder__Core.Person.t := Person.t;
      Common__Filter_builder__Core.Version.t := Version.t;
      Common__Filter_builder__Core.Set.t := Set.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Common.Filter_builder.Core.Book.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Any = struct
    type predicate = [%import: Common.Filter_builder.Core.Any.predicate [@with Common.Entry.Id.t := Id.t;
      Common.Model_builder.Core.Any.Type.t := Model.Any.Type.t;
      Common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Common.Kind.Version.Filter.t := Kind.Version.Filter.t;
      Common.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
      Common__Filter_builder__Core.Source.t := Source.t;
      Common__Filter_builder__Core.Person.t := Person.t;
      Common__Filter_builder__Core.Dance.t := Dance.t;
      Common__Filter_builder__Core.Book.t := Book.t;
      Common__Filter_builder__Core.Set.t := Set.t;
      Common__Filter_builder__Core.Tune.t := Tune.t;
      Common__Filter_builder__Core.Version.t := Version.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Common.Filter_builder.Core.Any.t [@with Common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end
end
