open QCheck2

module Id = struct
  type 'any t = [%import: 'any Dancelor_common.Entry.Id.t]

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
    (* convert to and id (which will check it), and we're done! *)
    pure @@ Option.get @@ Dancelor_common.Entry.Id.of_string str
end

module Entry = struct
  (* The following are dirty tricks, necessary to convince [ppx_deriving_qcheck]
     that it can generate an [t Id.t]. These “models” are only to be used in a
     context where we use their id; there, it is fine since [Id.gen] ignores its
     first argument. *)

  module User = struct
    (* FIXME: not sure this one is actually fine *)
    type t = Dancelor_common.Model_builder.Core.User.t
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end
end

module Formula = struct
  type 'p t = [%import: 'p Dancelor_common.Formula.t] [@@deriving qcheck2]

  (* Formulas can grow very quickly. We therefore limit the size of formulas
     drastically in our generator. *)
  let gen gen_p = gen_sized gen_p 10
end

module Formula_string = struct
  type predicate = [%import: Dancelor_common.Formula_string.predicate]
  [@@deriving qcheck2]

  type t = [%import: Dancelor_common.Formula_string.t [@with Dancelor_common.Formula.t := Formula.t;]
  ]
  [@@deriving qcheck2]
end

module Formula_list = struct
  type 'f predicate = [%import: 'f Dancelor_common.Formula_list.predicate]
  [@@deriving qcheck2]

  type 'f t = [%import: 'f Dancelor_common.Formula_list.t [@with Dancelor_common.Formula.t := Formula.t;]
  ]
  [@@deriving qcheck2]
end

module Formula_user = struct
  type predicate = [%import: Dancelor_common.Formula_user.predicate [@with Dancelor_common.Entry.Id.t := Id.t;
    Dancelor_common.Entry.id := Id.t;
    Dancelor_common.Model_builder.Core.User.t := Model.User.t;
    Dancelor_common.Formula_string.t := Formula_string.t;
    ]
  ]
  [@@deriving qcheck2]
  type t = [%import: Dancelor_common.Formula_user.t [@with Dancelor_common.Formula.t := Formula.t;]
  ]
  [@@deriving qcheck2]
end

module Formula_entry = struct
  type meta_predicate = [%import: Dancelor_common.Formula_entry.meta_predicate]
  [@@deriving qcheck2]

  type meta = [%import: Dancelor_common.Formula_entry.meta [@with Dancelor_common.Formula.t := Formula.t;]
  ]
  [@@deriving qcheck2]

  type ('value, 'filter, 'access_filter) predicate_gen = [%import: ('value, 'filter, 'access_filter) Dancelor_common.Formula_entry.predicate_gen [@with Dancelor_common.Entry.Id.t := Id.t;]
  ]
  [@@deriving qcheck2]

  type ('value, 'filter, 'access_filter) gen = [%import: ('value, 'filter, 'access_filter) Dancelor_common.Formula_entry.gen [@with Dancelor_common.Formula.t := Formula.t]
  ]
  [@@deriving qcheck2]

  type access_public_predicate = [%import: Dancelor_common.Formula_entry.access_public_predicate [@with Dancelor_common.Formula.t := Formula.t;
    Nes.Void.t := Void.t;
    ]
  ]
  [@@deriving qcheck2]

  type access_public = access_public_predicate Formula.t
  [@@deriving qcheck2]

  type ('value, 'filter) public = [%import: ('value, 'filter) Dancelor_common.Formula_entry.public [@with Dancelor_common.Formula.t := Formula.t;]
  ]
  [@@deriving qcheck2]

  type access_private_predicate = [%import: Dancelor_common.Formula_entry.access_private_predicate [@with Dancelor_common.Formula.t := Formula.t;
    public := public;
    Dancelor_common.Formula_list.t := Formula_list.t;
    Dancelor_common.Formula_user.t := Formula_user.t;
    Dancelor_common.Entry.User.t := Entry.User.t;
    Nes.Void.t := Void.t;
    ]
  ]
  [@@deriving qcheck2]

  type access_private = [%import: Dancelor_common.Formula_entry.access_private [@with Dancelor_common.Formula.t := Formula.t;]
  ]
  [@@deriving qcheck2]

  type ('value, 'filter) private_ = [%import: ('value, 'filter) Dancelor_common.Formula_entry.private_ [@with Dancelor_common.Formula.t := Formula.t;]
  ]
  [@@deriving qcheck2]
end

module Text_formula = struct
  type predicate = [%import: Dancelor_common.Text_formula.predicate]
  and t = [%import: Dancelor_common.Text_formula.t]

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
          (Dancelor_common.Text_formula.raw <$> string_printable);
          (Dancelor_common.Text_formula.nullary <$> gen_predicate_name);
          (Dancelor_common.Text_formula.unary <$> gen_predicate_name <*> Formula.gen (self ()));
        ]
      )
      ()

  let gen = Formula.gen gen_predicate
end

module Kind = struct
  module Base = struct
    type t = [%import: Dancelor_common.Kind.Base.t] [@@deriving qcheck2]

    module Filter = struct
      type predicate = [%import: Dancelor_common.Kind.Base.Filter.predicate] [@@deriving qcheck2]

      type t = [%import: Dancelor_common.Kind.Base.Filter.t [@with Dancelor_common.Formula.t := Formula.t;]
      ]
      [@@deriving qcheck2]
    end
  end

  module Version = struct
    type t = [%import: Dancelor_common.Kind.Version.t]

    let gen = Gen.(pair nat Base.gen)

    module Filter = struct
      type predicate = [%import: Dancelor_common.Kind.Version.Filter.predicate [@with Dancelor_common.Kind_base.Filter.t := Base.Filter.t;]
      ]
      [@@deriving qcheck2]

      type t = [%import: Dancelor_common.Kind.Version.Filter.t [@with Dancelor_common.Formula.t := Formula.t;]
      ]
      [@@deriving qcheck2]
    end
  end

  module Dance = struct
    type t = [%import: Dancelor_common.Kind.Dance.t]

    let gen =
      let open Gen in
      let open Dancelor_common.Kind.Dance in
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
      type predicate = [%import: Dancelor_common.Kind.Dance.Filter.predicate [@with Dancelor_common.Kind_version.Filter.t := Version.Filter.t;]
      ]
      [@@deriving qcheck2]

      type t = [%import: Dancelor_common.Kind.Dance.Filter.t [@with Dancelor_common.Formula.t := Formula.t;]
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
    type t = Dancelor_common.Model_builder.Core.Source.t
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Person = struct
    type t = Dancelor_common.Model_builder.Core.Person.t
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Dance = struct
    type t = Dancelor_common.Model_builder.Core.Dance.t
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Tune = struct
    type t = Dancelor_common.Model_builder.Core.Tune.t
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Version = struct
    type t = Dancelor_common.Model_builder.Core.Version.t
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Set = struct
    type t = Dancelor_common.Model_builder.Core.Set.t
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Book = struct
    type t = Dancelor_common.Model_builder.Core.Book.t
    let gen : t QCheck2.Gen.t = Gen.pure (Obj.magic 0)
  end

  module Any = struct
    module Type = struct
      type t = [%import: Dancelor_common.Model_builder.Core.Any.Type.t [@with Dancelor_common.Formula.t := Formula.t;]
      ]
      [@@deriving qcheck2]
    end
  end
end

module Filter = struct
  module Person = struct
    type predicate = [%import: Dancelor_common.Filter_builder.Core.Person.predicate [@with Dancelor_common.Entry.Id.t := Id.t;
      Dancelor_common.Entry.id := Id.t;
      Dancelor_common.Model_builder.Core.Person.t := Model.Person.t;
      Dancelor_common.Formula_string.t := Formula_string.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Dancelor_common.Filter_builder.Core.Person.t [@with Dancelor_common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Source = struct
    type predicate = [%import: Dancelor_common.Filter_builder.Core.Source.predicate [@with Dancelor_common.Entry.Id.t := Id.t;
      Dancelor_common.Entry.id := Id.t;
      Dancelor_common.Model_builder.Core.Person.t := Model.Person.t;
      Dancelor_common.Model_builder.Core.Source.t := Model.Source.t;
      Dancelor_common.Formula_string.t := Formula_string.t;
      Dancelor_common.Formula_list.t := Formula_list.t;
      Dancelor_common.Formula_entry.t := Formula_entry.t;
      Dancelor_common.Formula_entry.public := Formula_entry.public;
      Dancelor_common__Filter_builder__Core.Person.t := Person.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Dancelor_common.Filter_builder.Core.Source.t [@with Dancelor_common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Dance = struct
    type predicate = [%import: Dancelor_common.Filter_builder.Core.Dance.predicate [@with Dancelor_common.Entry.Id.t := Id.t;
      Dancelor_common.Entry.id := Id.t;
      Dancelor_common.Model_builder.Core.Person.t := Model.Person.t;
      Dancelor_common.Model_builder.Core.Dance.t := Model.Dance.t;
      Dancelor_common.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
      Dancelor_common.Formula_string.t := Formula_string.t;
      Dancelor_common.Formula_list.t := Formula_list.t;
      Dancelor_common.Formula_entry.t := Formula_entry.t;
      Dancelor_common.Formula_entry.public := Formula_entry.public;
      Dancelor_common__Filter_builder__Core.Person.t := Person.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Dancelor_common.Filter_builder.Core.Dance.t [@with Dancelor_common.Formula.t := Formula.t]
    ]
    [@@deriving qcheck2]
  end

  module Tune = struct
    type predicate = [%import: Dancelor_common.Filter_builder.Core.Tune.predicate [@with Dancelor_common.Entry.Id.t := Id.t;
      Dancelor_common.Model_builder.Core.Person.t := Model.Person.t;
      Dancelor_common.Model_builder.Core.Tune.t := Model.Tune.t;
      Dancelor_common.Model_builder.Core.Dance.t := Model.Dance.t;
      Dancelor_common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Dancelor_common.Formula_string.t := Formula_string.t;
      Dancelor_common.Formula_list.t := Formula_list.t;
      Dancelor_common.Formula_entry.t := Formula_entry.t;
      Dancelor_common.Formula_entry.public := Formula_entry.public;
      Dancelor_common__Filter_builder__Core.Person.t := Person.t;
      Dancelor_common__Filter_builder__Core.Dance.t := Dance.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Dancelor_common.Filter_builder.Core.Tune.t [@with Dancelor_common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Version = struct
    type predicate = [%import: Dancelor_common.Filter_builder.Core.Version.predicate [@with Dancelor_common.Entry.Id.t := Id.t;
      Dancelor_common.Model_builder.Core.Version.t := Model.Version.t;
      Dancelor_common.Model_builder.Core.Tune.t := Model.Tune.t;
      Dancelor_common.Model_builder.Core.Source.t := Model.Source.t;
      Dancelor_common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Dancelor_common.Kind.Version.Filter.t := Kind.Version.Filter.t;
      Dancelor_common.Formula_string.t := Formula_string.t;
      Dancelor_common.Formula_list.t := Formula_list.t;
      Dancelor_common.Formula_entry.t := Formula_entry.t;
      Dancelor_common.Formula_entry.public := Formula_entry.public;
      Dancelor_common__Filter_builder__Core.Source.t := Source.t;
      Dancelor_common__Filter_builder__Core.Person.t := Person.t;
      Dancelor_common__Filter_builder__Core.Dance.t := Dance.t;
      Dancelor_common__Filter_builder__Core.Tune.t := Tune.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Dancelor_common.Filter_builder.Core.Version.t [@with Dancelor_common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Set = struct
    type predicate = [%import: Dancelor_common.Filter_builder.Core.Set.predicate [@with Dancelor_common.Entry.Id.t := Id.t;
      Dancelor_common.Model_builder.Core.Version.t := Model.Version.t;
      Dancelor_common.Model_builder.Core.Person.t := Model.Person.t;
      Dancelor_common.Model_builder.Core.Set.t := Model.Set.t;
      Dancelor_common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Dancelor_common.Kind.Version.Filter.t := Kind.Version.Filter.t;
      Dancelor_common.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
      Dancelor_common.Formula_list.t := Formula_list.t;
      Dancelor_common.Formula_string.t := Formula_string.t;
      Dancelor_common.Formula_entry.t := Formula_entry.t;
      Dancelor_common.Formula_entry.public := Formula_entry.public;
      Dancelor_common.Formula_user.t := Formula_user.t;
      Dancelor_common.Entry.User.t := Entry.User.t;
      Dancelor_common__Filter_builder__Core.Person.t := Person.t;
      Dancelor_common__Filter_builder__Core.Version.t := Version.t;
      Dancelor_common__Filter_builder__Core.User.t := User.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Dancelor_common.Filter_builder.Core.Set.t [@with Dancelor_common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Book = struct
    type predicate = [%import: Dancelor_common.Filter_builder.Core.Book.predicate [@with Dancelor_common.Entry.Id.t := Id.t;
      Dancelor_common.Model_builder.Core.Version.t := Model.Version.t;
      Dancelor_common.Model_builder.Core.Person.t := Model.Person.t;
      Dancelor_common.Model_builder.Core.Book.t := Model.Book.t;
      Dancelor_common.Model_builder.Core.Set.t := Model.Set.t;
      Dancelor_common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Dancelor_common.Kind.Version.Filter.t := Kind.Version.Filter.t;
      Dancelor_common.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
      Dancelor_common.Formula_list.t := Formula_list.t;
      Dancelor_common.Formula_string.t := Formula_string.t;
      Dancelor_common.Formula_entry.t := Formula_entry.t;
      Dancelor_common.Formula_entry.public := Formula_entry.public;
      Dancelor_common.Formula_entry.private_ := Formula_entry.private_;
      Dancelor_common.Entry.User.t := Entry.User.t;
      Dancelor_common.Formula_user.t := Formula_user.t;
      Dancelor_common__Filter_builder__Core.Person.t := Person.t;
      Dancelor_common__Filter_builder__Core.Version.t := Version.t;
      Dancelor_common__Filter_builder__Core.Set.t := Set.t;
      Dancelor_common__Filter_builder__Core.User.t := User.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Dancelor_common.Filter_builder.Core.Book.t [@with Dancelor_common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module User = struct
    type predicate = [%import: Dancelor_common.Filter_builder.Core.User.predicate [@with Dancelor_common.Entry.Id.t := Id.t;
      Dancelor_common.Model_builder.Core.Version.t := Model.Version.t;
      Dancelor_common.Model_builder.Core.Person.t := Model.Person.t;
      Dancelor_common.Model_builder.Core.User.t := Model.User.t;
      Dancelor_common.Model_builder.Core.Set.t := Model.Set.t;
      Dancelor_common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Dancelor_common.Kind.Version.Filter.t := Kind.Version.Filter.t;
      Dancelor_common.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
      Dancelor_common.Formula_list.t := Formula_list.t;
      Dancelor_common.Formula_string.t := Formula_string.t;
      Dancelor_common.Formula_entry.t := Formula_entry.t;
      Dancelor_common.Formula_entry.public := Formula_entry.public;
      Dancelor_common.Formula_entry.private_ := Formula_entry.private_;
      Dancelor_common.Entry.User.t := Entry.User.t;
      Dancelor_common.Formula_user.t := Formula_user.t;
      Dancelor_common__Filter_builder__Core.Person.t := Person.t;
      Dancelor_common__Filter_builder__Core.Version.t := Version.t;
      Dancelor_common__Filter_builder__Core.Set.t := Set.t;
      Dancelor_common__Filter_builder__Core.User.t := User.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Dancelor_common.Filter_builder.Core.User.t [@with Dancelor_common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Any = struct
    type predicate = [%import: Dancelor_common.Filter_builder.Core.Any.predicate [@with Dancelor_common.Entry.Id.t := Id.t;
      Dancelor_common.Model_builder.Core.Version.t := Model.Version.t;
      Dancelor_common.Model_builder.Core.Person.t := Model.Person.t;
      Dancelor_common.Model_builder.Core.Tune.t := Model.Tune.t;
      Dancelor_common.Model_builder.Core.Dance.t := Model.Dance.t;
      Dancelor_common.Model_builder.Core.Set.t := Model.Set.t;
      Dancelor_common.Model_builder.Core.Book.t := Model.Book.t;
      Dancelor_common.Model_builder.Core.Source.t := Model.Source.t;
      Dancelor_common.Model_builder.Core.User.t := Entry.User.t;
      Dancelor_common.Model_builder.Core.Any.Type.t := Model.Any.Type.t;
      Dancelor_common.Kind.Base.Filter.t := Kind.Base.Filter.t;
      Dancelor_common.Kind.Version.Filter.t := Kind.Version.Filter.t;
      Dancelor_common.Kind.Dance.Filter.t := Kind.Dance.Filter.t;
      Dancelor_common.Formula.t := Formula.t;
      Dancelor_common.Formula_entry.t := Formula_entry.t;
      Dancelor_common.Formula_entry.gen := Formula_entry.gen;
      Dancelor_common.Formula_entry.public := Formula_entry.public;
      Dancelor_common.Formula_entry.private_ := Formula_entry.private_;
      Dancelor_common__Filter_builder__Core.Source.t := Source.t;
      Dancelor_common__Filter_builder__Core.Person.t := Person.t;
      Dancelor_common__Filter_builder__Core.Dance.t := Dance.t;
      Dancelor_common__Filter_builder__Core.Book.t := Book.t;
      Dancelor_common__Filter_builder__Core.Set.t := Set.t;
      Dancelor_common__Filter_builder__Core.Tune.t := Tune.t;
      Dancelor_common__Filter_builder__Core.Version.t := Version.t;
      Dancelor_common__Filter_builder__Core.User.t := User.t;
      ]
    ]
    [@@deriving qcheck2]
    type t = [%import: Dancelor_common.Filter_builder.Core.Any.t [@with Dancelor_common.Formula.t := Formula.t;]
    ]
    [@@deriving qcheck2]
  end

  module Any_no_neg = struct
    (* NOTE: This is a variant of {!Any} that cannot produce formulas with
       negation. This is because the Any predicates don't actually behave well
       with it. Ideally, we wouldn't resort to such “cheating”, but I just can't
       find a way to make them behave well, for the life of me. *)

    type predicate = Any.predicate
    type t = Any.t

    let rec remove_negation : 'a Formula.t -> 'a Formula.t = function
      | True | False | Pred _ as p -> p
      | Not f -> remove_negation f
      | Or (f1, f2) -> Or (remove_negation f1, remove_negation f2)
      | And (f1, f2) -> And (remove_negation f1, remove_negation f2)

    let gen = Gen.map remove_negation Any.gen
  end
end
