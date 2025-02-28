open Nes
open Html
open Js_of_ocaml

type error = Closed

let open_res make_page =
  let (promise, resolver) = Lwt.wait () in

  (* The actual [return] function requires a handle of the box to work, but the
     box itself needs to be able to call [return]; hence this dirty trick. *)
  let box_handle = ref None in
  let return v =
    match !box_handle with
    | None -> failwith "Dialog.open_: use of `return` before full initialisation"
    | Some box ->
      Lwt.wakeup_later resolver v;
      box##close;
      Dom.removeChild Dom_html.document##.body box
  in
  let page = make_page return in

  (* The HTML dialog box. *)
  let box =
    dialog
      [
        span
          ~a: [a_class ["close"]; a_onclick (fun _ -> return (Error Closed); false)]
          [
            i ~a: [a_class ["material-symbols-outlined"]] [txt "close"];
          ];
        div (Page.content page);
      ]
  in
  let dom_box = To_dom.of_dialog box in
  box_handle := Some dom_box;

  (* Add the box as a child of `<body>`. *)
  Dom.appendChild Dom_html.document##.body dom_box;
  dom_box##showModal;

  (* Add an event listener to close the box by clicking outside of it. *)
  ignore
    (
      let open Dom_html in
      addEventListener
        window
        Event.click
        (
          handler @@ fun event ->
          if event##.target = Js.some (dom_box :> element Js.t) then
            return (Error Closed);
          Js._true
        )
        Js._true
    );

  (* Return the promise of a result. *)
  promise

let open_ make_page =
  open_res (fun return -> make_page (return % Result.ok))
