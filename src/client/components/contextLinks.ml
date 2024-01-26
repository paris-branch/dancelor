open Nes
open Dancelor_client_html
open Dancelor_client_model
module PageRouter = Dancelor_common_pageRouter
module Utils = Dancelor_client_utils

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
        let%lwt scores = Lwt.map (snd % Result.get_ok) (search query) in
        let%lwt any = any_lwt in
        let (prev, _, next) = Option.get @@ List.find_context (fun score -> Any.equal (Score.value score) any) scores in
        Lwt.return @@ List.filter_map Fun.id [
          Option.map (Utils.AnyLink.make ~context [txt "Previous"] % Score.value) prev;
          Option.map (Utils.AnyLink.make ~context [txt "Next"] % Score.value) next;
        ]
      )
  );
