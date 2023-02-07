open Js_of_ocaml
open Js_of_ocaml_lwt

module Html = Dom_html

let js = Js.string

type modal = {
  element: Html.element Js.t;
  on_unfocus: (unit -> unit);
  targets: (Html.element Js.t ) list;
  on_refresh: (unit -> unit);
}

type t = {
  document: Html.document Js.t;
  body: Html.bodyElement Js.t;
  header: Html.element Js.t;
  mutable content: (Html.divElement Js.t ) option;
  mutable modals: modal list;
  mutable on_refresh: (unit -> unit);
}

type page = t

module type CONTENTS = sig
  type t
  val create : page -> t
  val contents : t -> Html.divElement Js.t
  val init : t -> unit
  val refresh : t -> unit
end

let create () =
  let document = Html.window##.document in
  let body = document##.body in
  let header = document##createElement (js "header") in
  let modals = [] in
  Dom.appendChild body header;
  let t = { document; body; header; content = None; modals; on_refresh = (fun () -> ()) } in
  Lwt.async
    (
      fun () ->
        Lwt_js_events.clicks
          ~use_capture: true
          document
          (
            fun ev _ ->
              Js.Opt.case
                ev##.target
                (fun () -> ())
                (
                  fun trg ->
                    List.map
                      (
                        fun modal ->
                          (
                            modal,
                            List.for_all
                              (fun modal_trg -> not (JsHelpers.is_child_of trg modal_trg))
                              modal.targets
                          )
                      )
                      t.modals
                    |> List.iter (fun (modal, unfocus) -> if unfocus then modal.on_unfocus ())
                );
              Lwt.return ()
          )
    );
  t

let document t =
  t.document

let set_header t contents =
  JsHelpers.clear_children t.header;
  Dom.appendChild t.header contents

let set_contents(type s) (module M: CONTENTS with type t = s) t contents =
  begin
    match t.content with
    | None -> Dom.appendChild t.body (M.contents contents)
    | Some c -> Dom.replaceChild t.body c (M.contents contents)
  end;
  (M.contents contents)##.classList##add (js "content");
  (M.contents contents)##.classList##add (js "page-body");
  t.content <- Some (M.contents contents);
  t.on_refresh <- (fun () -> M.refresh contents)

let register_modal ?(on_refresh = (fun () -> ())) t ~element ~on_unfocus ~targets =
  t.modals <- { element; on_unfocus; targets; on_refresh } :: t.modals

let remove_modal t element =
  t.modals <- List.filter (fun modal -> modal.element <> element) t.modals

let refresh t =
  t.on_refresh ();
  List.iter (fun (modal : modal) -> modal.on_refresh ()) t.modals
