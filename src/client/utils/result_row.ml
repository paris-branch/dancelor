open Nes
open React

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
  onclick: (unit -> unit Lwt.t) option;
  cells: cell list;
  classes: string list;
}

let make ?(classes = []) ?onclick cells =
  {onclick; cells; classes}

(** Generic row showing an emoji on the left and a message on the right. *)
let icon_row ?classes ?onclick icon message =
  let open Html in
  make
    ?classes
    ?onclick
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
  tr
    ~a: (
      List.filter_map id [
        Some (a_class t.classes);
        Option.map (fun f -> a_onclick (fun _ -> Lwt.async f; true)) t.onclick;
      ]
    )
    (
      List.map (fun cell -> R.td ~a: cell.a cell.content) t.cells
    )

let run_onclick row =
  Option.fold row.onclick ~none: lwt_unit ~some: (fun f -> f ())
