open Nes
open Dancelor_client_html
open Js_of_ocaml

(* FIXME: Should actually be a <dialog> element. This is only available from
   TyXML 4.5 though. *)

type error = Closed

let open_ content =
  let (promise, resolver) = Lwt.wait () in

  (* The actual [return] function requires a handle of the box to work, but the
     box itself needs to be able to call [return]; hence this dirty trick. *)
  let box_handle = ref None in
  let return v =
    match !box_handle with
    | None -> failwith "Dialog.open_: use of `return` before full initialisation"
    | Some box ->
      Lwt.wakeup_later resolver v;
      Dom.removeChild Dom_html.document##.body box
  in

  (* The HTML dialog box. FIXME: make it a <dialog>. *)
  let box =
    div
      ~a: [a_class ["dialog"]]
      [
        div
          ~a: [
            a_class ["content"];
          ]
          [
            span
              ~a: [a_class ["close"]; a_onclick (fun _ -> return (Error Closed); false)]
              [
                i ~a: [a_class ["material-symbols-outlined"]] [txt "close"];
              ];
            div (content (return % Result.ok))
          ]
      ]
  in
  let dom_box = To_dom.of_div box in
  box_handle := Some dom_box;

  (* Add the box as a child of `<body>`. *)
  Dom.appendChild Dom_html.document##.body dom_box;

  (* Add an event listener to close the box by clicking outside of it. *)
  ignore
    (
      let open Dom_html in
      addEventListener
        window
        Event.click
        (
          handler @@ fun event ->
          if event##.target = Js.some dom_box then
            return (Error Closed);
          Js._true
        )
        Js._true
    );

  (* Return the promise of a result. *)
  promise
