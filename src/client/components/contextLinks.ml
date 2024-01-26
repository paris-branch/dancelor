open Nes
open Dancelor_client_html
open Dancelor_client_model
module PageRouter = Dancelor_common_pageRouter
module Utils = Dancelor_client_utils

let make_context_link ~context ~left ~neighbour ~number_of_others =
  Fun.flip Option.map neighbour @@ fun neighbour ->
  Utils.AnyLink.make ~context ~a:[a_class ["context-link"; (if left then "context-link-left" else "context-link-right")]] [
    div ~a:[a_class ["context-link-aligner"]] [];
    div [
      div ~a:[a_class ["context-link-main"]] [txt @@ if left then "‹" else "›"];
      div ~a:[a_class ["context-link-detail"]] [txt Any.(Type.to_string (type_of neighbour))];
      div ~a:[a_class ["context-link-detail"]] [L.txt @@ Any.name neighbour];
      div ~a:[a_class ["context-link-detail"]] (
        if number_of_others <= 0 then [] else [txt @@ spf "...and %d more" number_of_others]);
    ];
  ] neighbour

let make_and_render ?context ~search any_lwt =
  L.div (
    match context with
    | None -> Lwt.return_nil
    | Some ((PageRouter.InSearch query) as context) ->
      (
        (* FIXME: In term of performances, this is awful because we redo the
           same search over and over again. We need caching of searches on
           the server side. *)
        (* FIXME: In term of performances (again), this is awful because we
           download all the results of all the search just to get the one
           before and after. We need an endpoint for this, or maybe we
           should save the index and just use a clever pagination. *)
        let%lwt (total, scores) = Lwt.map Result.get_ok (search query) in
        let scores = List.map Score.value scores in
        let%lwt any = any_lwt in
        let (prev, i, _, next) = Option.get @@ List.findi_context (Any.equal any) scores in
        Lwt.return @@ List.filter_map Fun.id [
          make_context_link ~context ~left:true ~neighbour:prev ~number_of_others:(i - 1);
          make_context_link ~context ~left:false ~neighbour:next ~number_of_others:(total - i - 2);
        ]
      )
  );
