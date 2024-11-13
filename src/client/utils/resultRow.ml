open Nes
open React
open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js

type action =
  | NoAction
  | Link of string S.t
  | Callback of (unit -> unit) (* FIXME: Lwt.t? *)
[@@deriving variants]

(** Our customised notion of cells. This is a glorified <td>, but this allows us
    to inspect it more easily. It will also allow for changes in the future. *)
type cell = {
  a: Html_types.td_attrib Html.attrib list;
  content: Html_types.td_content_fun Html.elt list S.t;
}

(* Sadly, it does not seem like we can easily coerce a type in ['a S.t], so we
   use this little trick instead. *)
let coerce_content
  : 'a. ([< Html_types.td_content_fun] as 'a) Html.elt list ->
  Html_types.td_content_fun Html.elt list
= fun x -> (x :> Html_types.td_content_fun Html.elt list)

let rcell
  : 'a 'b. ?a: ([< Html_types.td_attrib] as 'a) Html.attrib list ->
  ([< Html_types.td_content_fun] as 'b) Html.elt list S.t ->
  cell
= fun ?(a = []) content ->
  {
    a = (a :> Html_types.td_attrib Html.attrib list);
    content = (S.map coerce_content content);
  }

let cell ?a content = rcell ?a (S.const content)
let lcell ?a content = rcell ?a (Dancelor_client_html.S.from' [] content)

type t = {
  action: action;
  cells: cell list;
  classes: string list;
}

let make ?(classes = []) ?action ?href cells =
  let action =
    match (action, href) with
    | (None, None) -> NoAction
    | (Some a, None) -> a
    | (None, Some href) -> Link (S.const href)
    | (Some _, Some _) -> invalid_arg "Cannot have both action and href"
  in
  {action; cells; classes}

(** Generic row showing an emoji on the left and a message on the right. *)
let icon_row ?classes ?action icon message =
  let open Dancelor_client_html in
  make
    ?classes
    ?action
    [
      cell
        ~a: [a_colspan 9999]
        [
          i ~a: [a_class ["material-symbols-outlined"]] [txt icon];
          txt " ";
          txt message;
        ];
    ]

let to_clickable_row t =
  let open Dancelor_client_html in
  match t.action with
  | NoAction ->
    tr
      ~a: [a_class t.classes]
      (
        List.map (fun cell -> R.td ~a: cell.a cell.content) t.cells
      )
  | Link href ->
    tr
      ~a: [
        a_class ("clickable" :: t.classes);
      ]
      (
        List.map
          (fun cell ->
            td
              ~a: cell.a
              [
                a
                  ~a: [
                    a_class ["full-cell-link"];
                    R.a_href href;
                  ]
                  [
                    R.div
                      ~a: [a_class ["full-cell-link"]]
                      (
                        Fun.flip S.map cell.content @@ function
                          | [] -> [txt " "] (* empty cells would not have their link fill 100% of the height *)
                          | content -> content
                      )
                  ]
              ]
          )
          t.cells
      )
  | Callback f ->
    tr
      ~a: [
        a_class ("clickable" :: t.classes);
        a_onclick (fun _ -> f (); true);
      ]
      (
        List.map (fun cell -> R.td ~a: cell.a cell.content) t.cells
      )

let run_action row =
  match row.action with
  | NoAction -> ()
  | Link href -> Dom_html.window##.location##.href := Js.string (S.value href)
  | Callback f -> f ()
