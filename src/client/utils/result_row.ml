open Nes

type t = {
  onclick: (unit -> unit Lwt.t) option;
  cells: Html_types.td Html.elt list;
  classes: string list;
}

let make ?(classes = []) ?onclick cells =
  {onclick; cells; classes}

let to_clickable_row t =
  let open Html in
  tr
    ~a: (
      List.filter_map id [
        Some (a_class t.classes);
        Option.map (fun f -> a_onclick (fun _ -> Lwt.async f; true)) t.onclick;
      ]
    )
    (t.cells)
