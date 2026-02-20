open Nes
open Common

let rountrip_test ~name ~gen ~show ~to_string ~of_string ~equal =
  QCheck_alcotest.to_alcotest
    (
      QCheck2.Test.make
        ~count: 1_000
        ~long_factor: 50
        ~name
        ~print: (fun k ->
          "Kind:\n\n  " ^
          show k ^
          "\n\nRepr:\n\n  " ^
          to_string k ^
          "\n\nKind again:\n\n  " ^ (show (of_string (to_string k)))
        )
        gen
        (fun k -> (equal (of_string (to_string k)) k))
    )

let kind_base_to_short_string_of_string_roundtrip =
  rountrip_test
    ~name: "short"
    ~gen: QCheck_generators.Kind.Base.gen
    ~show: Kind.Base.show
    ~to_string: Kind.Base.to_short_string
    ~of_string: Kind.Base.of_string
    ~equal: Kind.Base.equal

let kind_base_to_long_string_of_string_roundtrip ~capitalised =
  rountrip_test
    ~name: (spf "long (capitalised = %b)" capitalised)
    ~gen: QCheck_generators.Kind.Base.gen
    ~show: Kind.Base.show
    ~to_string: Kind.Base.to_short_string
    ~of_string: Kind.Base.of_string
    ~equal: Kind.Base.equal

let kind_version_to_string_of_string_roundtrip =
  rountrip_test
    ~name: ""
    ~gen: QCheck_generators.Kind.Version.gen
    ~show: Kind.Version.show
    ~to_string: Kind.Version.to_string
    ~of_string: Kind.Version.of_string
    ~equal: Kind.Version.equal

let kind_dance_to_string_of_string_roundtrip =
  rountrip_test
    ~name: ""
    ~gen: QCheck_generators.Kind.Dance.gen
    ~show: Kind.Dance.show
    ~to_string: Kind.Dance.to_string
    ~of_string: Kind.Dance.of_string
    ~equal: Kind.Dance.equal

let () =
  Alcotest.run
    "kinds"
    [
      (
        "Kind.Base.(of_string % to_string) = id",
        [kind_base_to_short_string_of_string_roundtrip;
        kind_base_to_long_string_of_string_roundtrip ~capitalised: true;
        kind_base_to_long_string_of_string_roundtrip ~capitalised: false;
        ]
      );
      (
        "Kind.Version.(of_string % to_string) = id",
        [kind_version_to_string_of_string_roundtrip;
        ]
      );
      (
        "Kind.Dance.(of_string % to_string) = id",
        [kind_dance_to_string_of_string_roundtrip;
        ]
      );
    ]
