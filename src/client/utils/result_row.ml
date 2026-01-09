open Nes
open React

type action =
  | No_action
  | Callback of (unit -> unit Lwt.t)
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
let lcell ?a content = rcell ?a (Html.S.from' [] content)

type t = {
  action: action;
  cells: cell list;
  classes: string list;
}

let make ?(classes = []) ?action ?onclick cells =
  let action =
    match (action, onclick) with
    | (None, None) -> No_action
    | (Some a, None) -> a
    | (None, Some onclick) -> Callback onclick
    | _ -> invalid_arg "Cannot have more than one of action, href, or onclick"
  in
    {action; cells; classes}

(** Generic row showing an emoji on the left and a message on the right. *)
let icon_row ?classes ?action icon message =
  let open Html in
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
  let open Html in
  match t.action with
  | No_action ->
    tr
      ~a: [a_class t.classes]
      (
        List.map (fun cell -> R.td ~a: cell.a cell.content) t.cells
      )
  | Callback f ->
    tr
      ~a: [
        a_class t.classes;
        a_onclick (fun _ -> Lwt.async f; true);
      ]
      (
        List.map (fun cell -> R.td ~a: cell.a cell.content) t.cells
      )

let run_action row =
  match row.action with
  | No_action -> lwt_unit
  | Callback f -> f ()
