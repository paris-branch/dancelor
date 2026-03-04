open Nes

type predicate =
  | Title of Formula_string.t
  | Versions of (Model_builder.Core.Version.t, Version.t) Formula_entry.public Formula_list.t
  | Versions_deep of (Model_builder.Core.Version.t, Version.t) Formula_entry.public Formula_list.t
  | Sets of (Model_builder.Core.Set.t, Set.t) Formula_entry.private_ Formula_list.t
  | Editors of (Model_builder.Core.Person.t, Person.t) Formula_entry.public Formula_list.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let title' = Formula.pred % title
let versions' = Formula.pred % versions
let sets' = Formula.pred % sets
let versions_deep' = Formula.pred % versions_deep
let editors' = Formula.pred % editors

let converter =
  Text_formula_converter.(
    make
      ~raw: (ok % title' % Formula_string.matches')
      [
        unary_lift ~name: "title" (title, title_val) ~converter: Formula_string.converter;
        unary_lift ~name: "versions" (versions, versions_val) ~converter: (Formula_list.converter (Formula_entry.converter_public Version.converter));
        unary_lift ~name: "sets" (sets, sets_val) ~converter: (Formula_list.converter (Formula_entry.converter_private Set.converter));
        unary_lift ~name: "versions-deep" (versions_deep, versions_deep_val) ~converter: (Formula_list.converter (Formula_entry.converter_public Version.converter));
        unary_lift ~name: "editors" (editors, editors_val) ~converter: (Formula_list.converter (Formula_entry.converter_public Person.converter));
      ]
  )

let optimise =
  Formula.optimise
    ~up: (fun {is_tf} ->
      function
        | Title f -> is_tf f
        | Versions f -> is_tf f
        | Versions_deep f -> is_tf f
        | Sets f -> is_tf f
        | Editors f -> is_tf f
    )
    ~not_: (function
      | Title f -> some @@ title @@ Formula.not f
      | Versions f -> some @@ versions @@ Formula.not f
      | Versions_deep f -> some @@ versions_deep @@ Formula.not f
      | Sets f -> some @@ sets @@ Formula.not f
      | Editors f -> some @@ editors @@ Formula.not f
    )
    ~binop: (fun {op} f1 f2 ->
      match (f1, f2) with
      | (Title f1, Title f2) -> some @@ title (op f1 f2)
      | (Versions f1, Versions f2) -> some @@ versions (op f1 f2)
      | (Versions_deep f1, Versions_deep f2) -> some @@ versions_deep (op f1 f2)
      | (Sets f1, Sets f2) -> some @@ sets (op f1 f2)
      | (Editors f1, Editors f2) -> some @@ editors (op f1 f2)
      | _ -> None
    )
    (function
      | Title sfilter -> title @@ Formula_string.optimise sfilter
      | Versions vfilter -> versions @@ Formula_list.optimise (Formula_entry.optimise_public Version.optimise) vfilter
      | Versions_deep vfilter -> versions_deep @@ Formula_list.optimise (Formula_entry.optimise_public Version.optimise) vfilter
      | Sets sfilter -> sets @@ Formula_list.optimise (Formula_entry.optimise_private Set.optimise) sfilter
      | Editors pfilter -> editors @@ Formula_list.optimise (Formula_entry.optimise_public Person.optimise) pfilter
    )
