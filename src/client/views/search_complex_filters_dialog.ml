open Nes
open Dancelor_common
open Search_new
open Model
open Html
open Utils
open Components

(** Restricted predicates supported by the complex filter dialog. They are
    always of the form of a conjunction of disjunctions. *)
type restricted_predicate =
  | Person of (Person.t, Filter.Person.t) Formula_entry.predicate_public list list
  | Dance of (Dance.t, Filter.Dance.t) Formula_entry.predicate_public list list
  | Source of (Source.t, Filter.Source.t) Formula_entry.predicate_public list list
  | Book of (Book.t, Filter.Book.t) Formula_entry.predicate_private list list
  | Set of (Set.t, Filter.Set.t) Formula_entry.predicate_private list list
  | Tune of (Tune.t, Filter.Tune.t) Formula_entry.predicate_public list list
  | Version of (Version.t, Filter.Version.t) Formula_entry.predicate_public list list
[@@deriving variants]

(** Restricted formulas supported by the complex filter dialog. This is a bunch
    of raw strings and zero or one {!restricted_predicate}. *)
type restricted_formula = string list * restricted_predicate option

(** From a filter, return a {!restricted_formula}, or [None] if not possible. *)
let restrict_formula (text : string) : restricted_formula option =
  (* in the {!Option} monad, [None] being a failure to restrict the formula *)
  let%opt filter = Result.to_option @@ Text_formula.string_to_formula Filter.Any.converter text in
  let filter = Text_formula_converter.optimise Filter.Any.converter filter in
  (* special case for the formula that is just true *)
  if filter = Formula.True then
    Some ([], None)
  else
    (
      (* we only support conjunctions of predicates (after optimisation) *)
      let (preds, non_preds) =
        List.partition_map
          (function Formula.Pred p -> Left p | f -> Right f)
          (Formula.conjuncts filter)
      in
      (* assert%opt (non_preds = []);%opt *)
      let%opt () = if non_preds = [] then Some () else None in
      (* we separate the predicates between raw ones and non-raw ones *)
      let (raws, non_raws) =
        List.partition_map (function Filter.Any.Raw s -> Left s | p -> Right p) preds
      in
      (* there can be at most one non-raw predicate which must lift a model CNF *)
      let%opt pred =
        match non_raws with
        | [] -> Some None
        | [Person filter] -> Some (Option.map person (Formula.cnf_val filter))
        | [Dance filter] -> Some (Option.map dance (Formula.cnf_val filter))
        | [Source filter] -> Some (Option.map source (Formula.cnf_val filter))
        | [Book filter] -> Some (Option.map book (Formula.cnf_val filter))
        | [Set filter] -> Some (Option.map set (Formula.cnf_val filter))
        | [Tune filter] -> Some (Option.map tune (Formula.cnf_val filter))
        | [Version filter] -> Some (Option.map version (Formula.cnf_val filter))
        | _ -> None
      in
      (* return the raw predicates and the restricted lifted one *)
      Option.return (raws, pred)
    )

(** Takes a [~s]ignal to elements and a [~f]unction to map on those and return a
    signal to a filter representing those elements. *)
let choices_formula ~s ~f =
  S.flip_map s @@ function [] -> Formula.true_ | cs -> Formula.or_l (List.map f cs)

let type_choices (type_ : Any.Type.t option) =
  Choices.(
    make_radios
      ~label: "Type"
      [
        choice' [txt "All"] ~checked: (Option.is_none type_);
        choice' [txt "Person"] ~value: Any.Type.Person ~checked: (type_ = Some Any.Type.Person);
        choice' [txt "Dance"] ~value: Any.Type.Dance ~checked: (type_ = Some Any.Type.Dance);
        choice' [txt "Source"] ~value: Any.Type.Source ~checked: (type_ = Some Any.Type.Source);
        choice' [txt "Tune"] ~value: Any.Type.Tune ~checked: (type_ = Some Any.Type.Tune);
        choice' [txt "Version"] ~value: Any.Type.Version ~checked: (type_ = Some Any.Type.Version);
        choice' [txt "Set"] ~value: Any.Type.Set ~checked: (type_ = Some Any.Type.Set);
        choice' [txt "Book"] ~value: Any.Type.Book ~checked: (type_ = Some Any.Type.Book);
        choice' [txt "User"] ~value: Any.Type.User ~checked: (type_ = Some Any.Type.User);
      ]
  )

let make_person_specific_choices (_query : Person_query.specific option) = (S.const (), [])

let make_dance_specific_choices (_query : Dance_query.specific option) = (S.const {Dance_query.deviser = None}, [])

let make_source_specific_choices (_query : Source_query.specific option) = (S.const {Source_query.editor = None}, [])

let make_set_specific_choices (_query : Set_query.specific option) = (S.const {Set_query.conceptor = None}, [])

let make_book_specific_choices (_query : Book_query.specific option) = (S.const {Book_query.author = None}, [])

let make_tune_specific_choices (query : Tune_query.specific option) =
  let%lwt kind_choices =
    let checked kind =
      match query with
      | Some {kind = Some kinds; _} -> List.mem kind kinds
      | _ -> false
    in
    let open Choices in
    make_checkboxes ~label: "Kind" @@
      List.map
        (fun kind ->
          choice [txt (Kind.Base.to_long_string ~capitalised: true kind)] ~value: kind ~checked: (checked kind)
        )
        Kind.Base.all
  in
  let query =
    S.bind (S.map Result.get_ok @@ Component.signal kind_choices) @@ fun kind ->
    S.const {
      Tune_query.kind = if kind = [] then None else Some kind;
      composer = None; (* FIXME *)
    }
  in
  let html = [
    Component.inner_html kind_choices;
  ]
  in
  lwt (query, html)

let major_keys =
  let open Music in
  List.map
    (flip Key.make Major)
    [
      Pitch.make C Natural 0;
      Pitch.make G Natural 0;
      Pitch.make D Natural 0;
      Pitch.make A Natural 0;
      Pitch.make E Natural 0;
      Pitch.make B Natural 0;
      Pitch.make F Sharp 0;
      Pitch.make C Sharp 0;
      Pitch.make F Natural 0;
      Pitch.make B Flat 0;
      Pitch.make E Flat 0;
      Pitch.make A Flat 0;
      Pitch.make D Flat 0;
    ]

let major_key_choices (query : Version_query.specific option) =
  let checked key =
    match query with
    | Some {key = Some keys; _} -> List.mem key keys
    | _ -> false
  in
  Choices.(
    make_checkboxes
      ~label: "Major keys"
      (
        List.map
          (fun key ->
            choice [txt (Music.Key.to_pretty_string key)] ~value: key ~checked: (checked key)
          )
          major_keys
      )
  )

let minor_keys =
  let open Music in
  List.map
    (flip Key.make Minor)
    [
      Pitch.make A Natural 0;
      Pitch.make E Natural 0;
      Pitch.make B Natural 0;
      Pitch.make F Sharp 0;
      Pitch.make C Sharp 0;
      Pitch.make G Sharp 0;
      Pitch.make D Sharp 0;
      Pitch.make A Sharp 0;
      Pitch.make D Natural 0;
      Pitch.make G Natural 0;
      Pitch.make C Natural 0;
      Pitch.make F Natural 0;
      Pitch.make B Flat 0;
    ]

let minor_key_choices (query : Version_query.specific option) =
  let checked key =
    match query with
    | Some {key = Some keys; _} -> List.mem key keys
    | _ -> false
  in
  Choices.(
    make_checkboxes
      ~label: "Minor keys"
      (
        List.map
          (fun key ->
            choice [txt (Music.Key.to_pretty_string key)] ~value: key ~checked: (checked key)
          )
          minor_keys
      )
  )

let make_version_specific_choices ~tune_query ~tune_html (query : Version_query.specific option) =
  let%lwt major_key_choices = major_key_choices query in
  let%lwt minor_key_choices = minor_key_choices query in
  let key =
    S.l2
      (@)
      (S.map Result.get_ok (Component.signal major_key_choices))
      (S.map Result.get_ok (Component.signal minor_key_choices))
  in
  let query =
    S.bind tune_query @@ fun tune ->
    S.bind key @@ fun key ->
    S.const {
      Version_query.tune;
      key = if key = [] then None else Some key;
    }
  in
  let html =
    tune_html @ [
      Component.inner_html major_key_choices;
      Component.inner_html minor_key_choices;
    ]
  in
  lwt (query, html)

let make_user_specific_choices (_query : User_query.specific option) = (S.const (), [])

let make_common_choices (query : Query.common) = (S.const {Query.terms = query.terms}, [])

(* the dialog itself *)

let open_ (query : Any_query.t) =
  let (type_, person_query, dance_query, source_query, tune_query, version_query, set_query, book_query, user_query) =
    match query.specific with
    | None -> (None, None, None, None, None, None, None, None, None)
    | Some Person person_query -> (Some Any.Type.Person, Some person_query, None, None, None, None, None, None, None)
    | Some Dance dance_query -> (Some Any.Type.Dance, None, Some dance_query, None, None, None, None, None, None)
    | Some Source source_query -> (Some Any.Type.Source, None, None, Some source_query, None, None, None, None, None)
    | Some Tune tune_query -> (Some Any.Type.Tune, None, None, None, Some tune_query, None, None, None, None)
    | Some Version version_query -> (Some Any.Type.Version, None, None, None, Some version_query.tune, Some version_query, None, None, None)
    | Some Set set_query -> (Some Any.Type.Set, None, None, None, None, None, Some set_query, None, None)
    | Some Book book_query -> (Some Any.Type.Book, None, None, None, None, None, None, Some book_query, None)
    | Some User user_query -> (Some Any.Type.User, None, None, None, None, None, None, None, Some user_query)
  in
  let%lwt type_choices = type_choices type_ in
  let (person_query, person_html) = make_person_specific_choices person_query in
  let (dance_query, dance_html) = make_dance_specific_choices dance_query in
  let (source_query, source_html) = make_source_specific_choices source_query in
  let (book_query, book_html) = make_book_specific_choices book_query in
  let (set_query, set_html) = make_set_specific_choices set_query in
  let%lwt (tune_query, tune_html) = make_tune_specific_choices tune_query in
  let%lwt (version_query, version_html) = make_version_specific_choices version_query ~tune_query ~tune_html in
  let (user_query, user_html) = make_user_specific_choices user_query in
  let (common_query, common_html) = make_common_choices query.common in
  let specific =
    S.bind (S.map Result.get_ok @@ Component.signal type_choices) @@ function
      | None -> S.const None
      | Some Person -> S.map (some % Any_query.person) person_query
      | Some User -> S.map (some % Any_query.user) user_query
      | Some Dance -> S.map (some % Any_query.dance) dance_query
      | Some Source -> S.map (some % Any_query.source) source_query
      | Some Tune -> S.map (some % Any_query.tune) tune_query
      | Some Version -> S.map (some % Any_query.version) version_query
      | Some Set -> S.map (some % Any_query.set) set_query
      | Some Book -> S.map (some % Any_query.book) book_query
  in
  let new_query =
    S.bind common_query @@ fun common ->
    S.bind specific @@ fun specific ->
    S.const {Query.common; specific}
  in
  Page.open_dialog @@ fun return ->
  Page.make'
    ~title: (lwt "Complex filters")
    [div
      ~a: [a_class ["d-flex"; "justify-content-center"]]
      [
        Component.inner_html type_choices
      ];
    hr ();
    R.div
      ~a: [a_class ["d-flex"; "justify-content-center"]]
      (
        S.flip_map (S.map Result.get_ok (Component.signal type_choices)) @@ function
          | None -> []
          | Some Person -> person_html
          | Some Dance -> dance_html
          | Some Source -> source_html
          | Some Book -> book_html
          | Some Set -> set_html
          | Some Tune -> tune_html
          | Some Version -> version_html
          | Some User -> user_html
      );
    hr ();
    div ~a: [a_class ["d-flex"; "justify-content-center"]] common_html;
    ]
    ~buttons: [
      Button.cancel ~onclick: (fun () -> return None; lwt_unit) ();
      Button.clear ~onclick: (fun () -> lwt @@ return @@ Some Any_query.empty) ();
      Button.make
        ~label: "Apply"
        ~label_processing: "Applying..."
        ~icon: (Action Apply)
        ~classes: ["btn-primary"]
        ~onclick: (fun () -> return (some @@ S.value new_query); lwt_unit)
        ()
    ]

let open_error msg =
  Page.open_dialog' @@ fun return ->
  Page.make'
    ~title: (lwt "Complex filters")
    [p [txtf "It is not possible to open the filter dialog when the search text isn't valid. The search text is: %s" msg]]
    ~buttons: [
      a
        ~a: [
          a_class ["button"];
          a_onclick (fun _ -> return (); false);
        ]
        [txt "OK"];
    ]

let open_ text =
  match Any_query.parse text with
  | Error msg -> const None <$> open_error msg
  | Ok query -> open_ query
