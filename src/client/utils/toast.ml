open Nes
open Html
open Js_of_ocaml

let stack =
  let dom_elt = ref None in
  fun () ->
    match !dom_elt with
    | Some elt -> elt
    | None ->
      let elt = To_dom.of_div @@ div ~a: [a_class ["toast-container"; "position-fixed"; "top-0"; "end-0"; "p-3"]] [] in
      dom_elt := Some elt;
      Dom.appendChild Dom_html.document##.body elt;
      elt

type level =
  | Normal
  | Warning

let level_to_class = function
  | Normal -> ([], [])
  | Warning -> (["bg-warning"; "text-dark"], ["bg-warning-subtle"])

let open_ ?(level = Normal) ~title ?(buttons = []) content =
  let start = Unix.time () in
  let (class_header, class_body) = level_to_class level in
  let toast =
    To_dom.of_div @@
      div
        ~a: [a_class ["toast"; "fade"; "show"]; a_role ["alert"]; a_aria "live" ["assertive"]; a_aria "atomic" ["true"]]
        [
          div
            ~a: [a_class (["toast-header"] @ class_header)]
            [
              h6 ~a: [a_class ["me-auto"; "my-0"]] [txt title];
              small [R.txt (Time.ago_s start)];
              button ~a: [a_button_type `Button; a_class ["btn-close"]; a_user_data "bs-dismiss" "toast"; a_aria "label" ["Close"]] [];
            ];
          div ~a: [a_class (["toast-body"] @ class_body)] (
            content @ [
              div ~a: [a_class ["mt-2"; "pt-2"; "border-top"; "text-end"]] (
                List.intersperse
                  (txt " ")
                  (Button.close ~more_a: [a_user_data "bs-dismiss" "toast"] () :: buttons)
              )
            ]
          );
        ]
  in
  Lwt.async (fun () ->
    Js_of_ocaml_lwt.Lwt_js.sleep 60.;%lwt
    Dom.removeChild (stack ()) toast;
    lwt_unit
  );
  Dom.appendChild (stack ()) toast
