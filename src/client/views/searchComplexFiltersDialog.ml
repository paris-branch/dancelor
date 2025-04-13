open Nes
open Common

open Model
open Html
open Components

(** Restricted predicates supported by the complex filter dialog. They are
    always of the form of a conjunction of disjunctions. *)
type restricted_predicate =
  | Source of Source.Filter.predicate list list
  | Person of Person.Filter.predicate list list
  | Dance of Dance.Filter.predicate list list
  | Book of Book.Filter.predicate list list
  | Set of Set.Filter.predicate list list
  | Tune of Tune.Filter.predicate list list
  | Version of Version.Filter.predicate list list
[@@deriving variants]

(** Restricted formulas supported by the complex filter dialog. This is a bunch
    of raw strings and zero or one {!restricted_predicate}. *)
type restricted_formula = string list * restricted_predicate option

(** From a filter, return a {!restricted_formula}, or [None] if not possible. *)
let restrict_formula (text : string) : restricted_formula option =
  (* in the {!Option} monad, [None] being a failure to restrict the formula *)
  let%opt filter = Result.to_option (Any.Filter.from_string text) in
  let filter = Any.Filter.optimise filter in
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
        List.partition_map (function Any.Filter.Raw s -> Left s | p -> Right p) preds
      in
      (* there can be at most one non-raw predicate which must lift a model CNF *)
      let%opt pred =
        match non_raws with
        | [] -> Some None
        | [Type Source] -> Some (Some (source []))
        | [Source filter] -> Some (Option.map source (Formula.unCnf filter))
        | [Type Person] -> Some (Some (person []))
        | [Person filter] -> Some (Option.map person (Formula.unCnf filter))
        | [Type Dance] -> Some (Some (dance []))
        | [Dance filter] -> Some (Option.map dance (Formula.unCnf filter))
        | [Type Book] -> Some (Some (book []))
        | [Book filter] -> Some (Option.map book (Formula.unCnf filter))
        | [Type Set] -> Some (Some (set []))
        | [Set filter] -> Some (Option.map set (Formula.unCnf filter))
        | [Type Tune] -> Some (Some (tune []))
        | [Tune filter] -> Some (Option.map tune (Formula.unCnf filter))
        | [Type Version] -> Some (Some (version []))
        | [Version filter] -> Some (Option.map version (Formula.unCnf filter))
        | _ -> None
      in
      (* return the raw predicates and the restricted lifted one *)
      Option.return (raws, pred)
    )

(** Takes a [~s]ignal to elements and a [~f]unction to map on those and return a
    signal to a filter representing those elements. *)
let choices_formula ~s ~f =
  Fun.flip S.map s @@ function [] -> Formula.true_ | cs -> Formula.or_l (List.map f cs)

let type_choices filter =
  let checked =
    match filter with
    | None -> Fun.const false
    | Some filter ->
      fun type_ ->
        match (type_, filter) with
        | (Any.Type.Source, Source _) -> true
        | (Any.Type.Person, Person _) -> true
        | (Any.Type.Dance, Dance _) -> true
        | (Any.Type.Book, Book _) -> true
        | (Any.Type.Set, Set _) -> true
        | (Any.Type.Tune, Tune _) -> true
        | (Any.Type.Version, Version _) -> true
        | _ -> false
  in
  Choices.(
    make_radios
      (
        choice' [txt "All"] ~checked: (filter = None) :: List.map
          (fun type_ ->
            choice' [txt (Any.Type.to_string type_)] ~value: type_ ~checked: (checked type_)
          )
          Any.Type.all
      )
  )

let kind_choices filter =
  let checked kind =
    (* NOTE: Will stop working when we push the disjunction inside the kind filters *)
    match filter with
    | Some (Dance filter) -> List.exists (List.mem (Dance.Filter.kind @@ Kind.Dance.Filter.baseIs' kind)) filter
    | Some (Set filter) -> List.exists (List.mem (Set.Filter.kind @@ Kind.Dance.Filter.baseIs' kind)) filter
    | Some (Tune filter) -> List.exists (List.mem (Tune.Filter.kind @@ Kind.Base.Filter.is' kind)) filter
    | Some (Version filter) -> List.exists (List.mem (Version.Filter.kind @@ Kind.Version.Filter.baseIs' kind)) filter
    | _ -> false
  in
  Choices.(
    make_checkboxes
      (
        List.map
          (fun kind ->
            choice
              [txt (Kind.Base.to_pretty_string ~capitalised: true kind)]
              ~value: kind
              ~checked: (checked kind)
          )
          Kind.Base.all
      )
  )

(* source-specific choices *)

let source_bundled_choices _filter = (S.const Formula.true_, [])

(* person-specific choices *)

let person_bundled_choices _filter = (S.const Formula.true_, [])

(* dance-specific choices *)

let dance_bundled_choices ~kind_choices _filter =
  let formula =
    S.map (Any.Filter.dance' % Formula.and_l) @@
      S.all
        [
          choices_formula
            ~s: (Choices.signal kind_choices)
            ~f: (Dance.Filter.kind' % Kind.Dance.Filter.baseIs');
        ]
  in
  let html = [
    Choices.render kind_choices;
  ]
  in
    (formula, html)

(* book-specific choices *)

let book_bundled_choices _filter = (S.const Formula.true_, [])

(* set-specific choices *)

let set_bundled_choices ~kind_choices _filter =
  let formula =
    S.map (Any.Filter.set' % Formula.and_l) @@
      S.all
        [
          choices_formula
            ~s: (Choices.signal kind_choices)
            ~f: (Set.Filter.kind' % Kind.Dance.Filter.baseIs');
        ]
  in
  let html = [
    Choices.render kind_choices;
  ]
  in
    (formula, html)

(* tune-specific choices *)

let tune_bundled_choices ~kind_choices _filter =
  let formula =
    S.map (Any.Filter.tune' % Formula.and_l) @@
      S.all
        [
          choices_formula
            ~s: (Choices.signal kind_choices)
            ~f: (Tune.Filter.kind' % Kind.Base.Filter.is');
        ]
  in
  let html = [
    Choices.render kind_choices;
  ]
  in
    (formula, html)

(* version-specific choices *)

let major_keys =
  let open Music in
  List.map
    (Fun.flip make_key Major)
    [
      make_pitch C Natural 0;
      make_pitch G Natural 0;
      make_pitch D Natural 0;
      make_pitch A Natural 0;
      make_pitch E Natural 0;
      make_pitch B Natural 0;
      make_pitch F Sharp 0;
      make_pitch C Sharp 0;
      make_pitch F Natural 0;
      make_pitch B Flat 0;
      make_pitch E Flat 0;
      make_pitch A Flat 0;
      make_pitch D Flat 0;
    ]

let major_key_choices filter =
  let checked key =
    (* NOTE: Will stop working when we introduce key filters *)
    match filter with
    | Some (Version filter) -> List.exists (List.mem (Version.Filter.key key)) filter
    | _ -> false
  in
  Choices.(
    make_checkboxes
      (
        List.map
          (fun key ->
            choice
              [txt (Music.key_to_pretty_string key)]
              ~value: key
              ~checked: (checked key)
          )
          major_keys
      )
  )

let minor_keys =
  let open Music in
  List.map
    (Fun.flip make_key Minor)
    [
      make_pitch A Natural 0;
      make_pitch E Natural 0;
      make_pitch B Natural 0;
      make_pitch F Sharp 0;
      make_pitch C Sharp 0;
      make_pitch G Sharp 0;
      make_pitch D Sharp 0;
      make_pitch A Sharp 0;
      make_pitch D Natural 0;
      make_pitch G Natural 0;
      make_pitch C Natural 0;
      make_pitch F Natural 0;
      make_pitch B Flat 0;
    ]

let minor_key_choices filter =
  let checked key =
    (* NOTE: Will stop working when we introduce key filters *)
    match filter with
    | Some (Version filter) -> List.exists (List.mem (Version.Filter.key key)) filter
    | _ -> false
  in
  Choices.(
    make_checkboxes
      (
        List.map
          (fun key ->
            choice
              [txt (Music.key_to_pretty_string key)]
              ~value: key
              ~checked: (checked key)
          )
          minor_keys
      )
  )

let version_bundled_choices ~kind_choices filter =
  let major_key_choices = major_key_choices filter in
  let minor_key_choices = minor_key_choices filter in
  let formula =
    S.map (Any.Filter.version' % Formula.and_l) @@
      S.all
        [
          choices_formula
            ~s: (Choices.signal kind_choices)
            ~f: (Version.Filter.kind' % Kind.Version.Filter.baseIs');
          choices_formula
            ~s: (S.l2 (@) (Choices.signal major_key_choices) (Choices.signal minor_key_choices))
            ~f: Version.Filter.key';
        ]
  in
  let html = [
    Choices.render kind_choices;
    Choices.render major_key_choices;
    Choices.render minor_key_choices;
  ]
  in
    (formula, html)

(* the dialog itself *)

let open_ text raws filter =
  let type_choices = type_choices filter in

  (* bundled choices shared between models *)
  let kind_choices = kind_choices filter in

  (* model-specific bundled choices *)
  let (source_formula, source_html) = source_bundled_choices filter in
  let (person_formula, person_html) = person_bundled_choices filter in
  let (dance_formula, dance_html) = dance_bundled_choices filter ~kind_choices in
  let (book_formula, book_html) = book_bundled_choices filter in
  let (set_formula, set_html) = set_bundled_choices filter ~kind_choices in
  let (tune_formula, tune_html) = tune_bundled_choices filter ~kind_choices in
  let (version_formula, version_html) = version_bundled_choices filter ~kind_choices in
  let new_filter =
    (* big conjunction *)
    S.map Formula.and_l @@
      S.all
        [
          (
            (* [type:version] if any type has been selected *)
            Fun.flip S.map (Choices.signal type_choices) @@ function
              | None -> Formula.true_
              | Some type_ -> Any.Filter.type_' type_
          );

          (* model-specific formulas *)
          (
            S.bind (Choices.signal type_choices) @@ function
              | None -> S.const Formula.true_
              | Some Source -> source_formula
              | Some Person -> person_formula
              | Some Dance -> dance_formula
              | Some Book -> book_formula
              | Some Set -> set_formula
              | Some Tune -> tune_formula
              | Some Version -> version_formula
          );

          (* a conjunction of the raw strings *)
          S.const (Formula.and_l (List.map Any.Filter.raw' raws));
        ]
  in
  Page.open_dialog' @@ fun return ->
  Page.make
    ~title: (S.const "Complex filters")
    [div
      ~a: [a_class ["d-flex"; "justify-content-center"]]
      [
        Choices.render type_choices
      ];
    hr ();
    R.div
      ~a: [a_class ["d-flex"; "justify-content-center"]]
      (
        Fun.flip S.map (Choices.signal type_choices) @@ function
          | None -> []
          | Some Source -> source_html
          | Some Person -> person_html
          | Some Dance -> dance_html
          | Some Book -> book_html
          | Some Set -> set_html
          | Some Tune -> tune_html
          | Some Version -> version_html
      );
    ]
    ~buttons: [
      Button.cancel ~onclick: (fun () -> return text; Lwt.return_unit) ();
      Button.clear ~onclick: (fun () -> return "") ();
      Button.make
        ~label: "Apply"
        ~label_processing: "Applying..."
        ~icon: "check-circle"
        ~classes: ["btn-primary"]
        ~onclick: (fun () -> return (Any.Filter.to_pretty_string @@ S.value new_filter); Lwt.return_unit)
        ()
    ]

let open_error () =
  Page.open_dialog' @@ fun return ->
  Page.make
    ~title: (S.const "Complex filters")
    [p [txt "You have nothing to learn from me anymore :') Fly, little bird, fly!"];
    p
      [
        txt
          "The formula is too complex for the complex filter dialog to \
             understand. If you think that this is a mistake, contact your \
             administrator."
      ];
    ]
    ~buttons: [
      a
        ~a: [
          a_class ["button"];
          a_onclick (fun _ -> return (); false);
        ]
        [txt "OK"];
    ]

let open_ text =
  match restrict_formula text with
  | None -> Lwt.map (Fun.const None) (open_error ())
  | Some (raws, filter) -> open_ text raws filter
