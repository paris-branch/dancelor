open Nes
open Html
open Js_of_ocaml

type t = {
  parent_title: string;
  before_title: Html_types.div_content_fun elt list;
  title: string S.t;
  content: Html_types.div_content_fun elt list;
  buttons: Html_types.div_content_fun elt list;
}

let full_title p =
  Fun.flip S.map p.title @@ function
  | "" -> p.parent_title
  | title ->
    match p.parent_title with
    | "" -> title
    | _ -> title ^ " | " ^ p.parent_title

let make
    ?(parent_title = "")
    ~title
    ?(before_title = [])
    ?(buttons = [])
    content
  = {
    parent_title;
    before_title;
    title;
    content;
    buttons;
  }

let render p =
  div
    [
      div p.before_title;
      div
        ~a: [a_class ["container"]]
        [
          h2 ~a: [a_class ["text-center"; "mb-4"]] [R.txt p.title];
          div p.content;
          div ~a: [a_class ["d-flex"; "justify-content-end"; "mt-4"]] p.buttons
        ];
    ]

let open_dialog make_page =
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
  let page = make_page return in

  (* The HTML dialog box. *)
  let box =
    (* FIXME: the `d-block` is a hack to make the element show; we should figure
       out how to tell Bootstrap to show the element directly *)
    div
      ~a: [a_class ["modal"; "fade"; "show"; "d-block"]; a_tabindex (-1)]
      [
        div
          ~a: [a_class ["modal-dialog"; "modal-lg"; "modal-fullscreen-lg-down"; "modal-dialog-centered"]]
          [
            div
              ~a: [a_class ["modal-content"]]
              [
                div
                  ~a: [a_class ["modal-header"]]
                  [
                    h4 ~a: [a_class ["modal-title"]] [R.txt page.title];
                    button
                      ~a: [
                        a_button_type `Button;
                        a_class ["btn-close"];
                        a_user_data "bs-dismiss" "modal";
                        a_aria "label" ["Close"];
                        a_onclick (fun _ -> return None; false);
                      ]
                      [];
                  ];
                div ~a: [a_class ["modal-body"]] (page.before_title @ page.content);
                div ~a: [a_class ["modal-footer"]] page.buttons;
              ];
          ];
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
          if event##.target = Js.some (dom_box :> element Js.t) then
            return None;
          Js._true
        )
        Js._true
    );

  (* Return the promise of a result. *)
  promise

let open_dialog' make_page =
  open_dialog (fun return -> make_page (return % Option.some))
