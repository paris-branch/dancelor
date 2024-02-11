open Dancelor_server_model

let kindDance_to_string_of_string_roundtrip =
  QCheck_alcotest.to_alcotest
    (
      QCheck.Test.make ~name: "Kind.Dance"
        (
          QCheck.make
            ~print: (fun k -> "Kind:\n\n  " ^ Kind.Dance.show k
                              ^ "\n\nRepr:\n\n  " ^ Kind.Dance.to_string k
                              ^ "\n\nKind again:\n\n  " ^ Kind.Dance.(show (of_string (to_string k))))
            ~shrink: Kind.Dance.shrink
            Kind.Dance.gen
        )
        (fun k -> Kind.Dance.(equal (of_string (to_string k)) k))
    )

let () =
  Alcotest.run
    "kinds"
    [
      ("of_string % to_string = id", [
          kindDance_to_string_of_string_roundtrip
        ]);
    ]
