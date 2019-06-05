open Js_of_ocaml
open Js_of_ocaml_lwt

module Html = Dom_html

let js = Js.string

type modal =
{
  element : Html.element Js.t;
  on_unfocus : (unit -> unit);
  targets : (Html.element Js.t) list;
}

type t =
{
  document : Html.document Js.t;
  body : Html.bodyElement Js.t;
  header : Html.element Js.t;
  mutable content : (Html.divElement Js.t) option;
  mutable modals : modal list;
}

let create () =
  let document = Html.window##.document in
  let body = document##.body in
  let header = document##createElement (js "header") in
  let modals = [] in
  Dom.appendChild body header;
  let t = {document; body; header; content = None; modals} in
  Lwt.async (fun () -> Lwt_js_events.clicks ~use_capture:true document
    (fun ev _ ->
      Js.Opt.case ev##.target
        (fun () -> ())
        (fun trg ->
          List.map (fun modal ->
            (modal,
              List.for_all
                (fun modal_trg -> not (JsHelpers.is_child_of trg modal_trg))
                modal.targets)) t.modals
          |> List.iter (fun (modal, unfocus) -> if unfocus then modal.on_unfocus ()));
      Lwt.return ()));
  t

let document t =
  t.document

let set_header t contents =
  JsHelpers.clear_children t.header;
  Dom.appendChild t.header contents

let set_contents t contents =
  begin match t.content with
  | None -> Dom.appendChild t.body contents
  | Some c -> Dom.replaceChild t.body c contents
  end;
  contents##.classList##add (js "content");
  contents##.classList##add (js "page-body");
  t.content <- Some contents

let register_modal t ~element ~on_unfocus ~targets =
  t.modals <- {element; on_unfocus; targets} :: t.modals

let remove_modal t element =
  t.modals <- List.filter (fun modal -> modal.element <> element) t.modals
