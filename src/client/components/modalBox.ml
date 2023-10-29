(* REVIEW: Should we use the <dialog> HTML tag? *)

let make content =
  let open Dancelor_client_html in

  (* Reactive signal that holds the information on whether the modal box should
     be visible or not. *)
  let (visible, set_visible) = S.create false in

  (* The actual modal box. This looks like:

         <div class="modal visible">
           <div class="modal-content">
             <span class="close">⨉</span>
             ...content...
           </div>
         </div>
  *)
  let box =
    div
      ~a:[
        R.a_class (
          Fun.flip S.map visible @@ function
          | false -> ["modal"]
          | true -> ["modal"; "visible"]
        );
      ]
      [
        div
          ~a:[
            a_class ["modal-content"];
          ]
          (
            span
              ~a:[
                a_class ["close"];
                a_onclick (fun _ -> set_visible false; false);
              ]
              [
                txt "⨉"
              ]
            :: content
          )
      ]
  in

  (* An event listener on the whole window such that if one clicks outside the
     box then the box disappears. *)
  ignore
    (
      let open Js_of_ocaml in
      let open Dom_html in
      addEventListener
        window
        Event.click
        (handler @@ fun event ->
         if event##.target = Js.some (To_dom.of_div box) then
           set_visible false;
         Js._true
        )
        Js._true
    );

  (* We return the box and a function to make it appear. *)
  (box, (fun () -> set_visible true))
