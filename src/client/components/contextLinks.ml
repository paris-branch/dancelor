open Nes
open Dancelor_client_html
open Dancelor_client_model
open Js_of_ocaml
open Js_of_ocaml_lwt
module PageRouter = Dancelor_common_pageRouter
module Utils = Dancelor_client_utils

let register_body_keydown_listener f =
  let rec body_keydown_listener () =
    Lwt_js_events.keydowns Dom_html.document##.body @@ fun ev _thread ->
    f ev;
    body_keydown_listener ()
  in
  Lwt.async body_keydown_listener

type swipe_direction = Up | Down | Left | Right

(* REVIEW: This first version seems a bit buggy. Sometimes, the `touches_0`
   variable gets a “TypeError: touches_0 is null” in the
   `body_touchend_listener`, killing the async thread. I feel that I am handling
   them correctly with `Js.Optdef.to_option` so I would bet on a bug of
   Js_of_ocaml? We should try again with a more recent version. *)
let register_body_swipe_listener f =
  (* Helper to get touch info. JS equivalent of `e.touches[0]`. *)
  let touches_0 ev = Js.Optdef.to_option (ev##.touches##item 0) in
  (* Helper to decide whether something is indeed a swipe. *)
  let detect_swipe duration move_x move_y =
    let duration_threshold = 0.5 in
    let move_threshold = 100 in
    if duration <= duration_threshold then
      (
        if move_x >= move_threshold then
          Some Right
        else if move_x <= - move_threshold then
          Some Left
        else if move_y >= move_threshold then
          Some Down
        else if move_y <= - move_threshold then
          Some Up
        else
          None
      )
    else
      None
  in
  (* Keep listeners in the background listening to touchstarts. *)
  let start_time = ref min_float in
  let start_x = ref 0 in
  let start_y = ref 0 in
  let rec body_touchstart_listener () =
    let%lwt ev = Lwt_js_events.touchstart Dom_html.document##.body in
    Fun.flip Option.iter (touches_0 ev) (fun touches_0 ->
        start_x := touches_0##.clientX;
        start_y := touches_0##.clientY;
        start_time := Unix.gettimeofday ()
      );
    body_touchstart_listener ()
  in
  (* And now touchends *)
  let rec body_touchend_listener () =
    let%lwt ev = Lwt_js_events.touchend Dom_html.document##.body in
    Fun.flip Option.iter (touches_0 ev) (fun touches_0 ->
        let end_x = touches_0##.clientX in
        let end_y = touches_0##.clientY in
        let end_time = Unix.gettimeofday () in
        Option.iter f (detect_swipe (end_time -. !start_time) (end_x - !start_x) (end_y - !start_y));
      );
    body_touchend_listener ()
  in
  Lwt.async body_touchstart_listener;
  Lwt.async body_touchend_listener

let make_context_link ~context ~left ~neighbour ~number_of_others =
  Fun.flip Option.map neighbour @@ fun neighbour ->
  let href = PageRouter.path_any ~context neighbour in
  register_body_keydown_listener (fun ev ->
      if ev##.keyCode = (if left then 37 else 39) then
        Dom_html.window##.location##.href := Js.string href);
  register_body_swipe_listener (fun dir ->
      if dir = (if left then Right else Left) then
        Dom_html.window##.location##.href := Js.string href);
  a
    ~a:[
      a_href href;
      a_class ["context-link"; (if left then "context-link-left" else "context-link-right")];
    ]
    [
      div ~a:[a_class ["context-link-aligner"]] [];
      let context_repr = match context with
        | InSearch query -> [[txt "In search for"]; [txt query]]
      in
      let element_repr = [
        [txt Any.(Type.to_string (type_of neighbour))];
        [L.txt @@ Any.name neighbour];
      ] @ (if number_of_others <= 0 then [] else [[txt @@ spf "...and %d more" number_of_others]])
      in
      div (
        List.map (div ~a:[a_class ["context-link-detail"]]) context_repr
        @ [div ~a:[a_class ["context-link-main"]] [txt @@ if left then "‹" else "›"]]
        @ List.map (div ~a:[a_class ["context-link-detail"]]) element_repr
      );
    ]

let make_and_render ?context ~search any_lwt =
  L.div (
    match context with
    | None -> Lwt.return_nil
    | Some ((PageRouter.InSearch query) as context) ->
      let%lwt (total, scores) = Lwt.map Result.get_ok (search query) in
      let scores = List.map Score.value scores in
      let%lwt any = any_lwt in
      let (prev, i, _, next) = Option.get @@ List.findi_context (Any.equal any) scores in
      Lwt.return @@ List.filter_map Fun.id [
        make_context_link ~context ~left:true ~neighbour:prev ~number_of_others:(i - 1);
        make_context_link ~context ~left:false ~neighbour:next ~number_of_others:(total - i - 2);
      ]
  );
