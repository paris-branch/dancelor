(** {1 Madge Router} *)

open Nes

type request =
  { method_ : Cohttp.Code.meth ;
    path : string ;
    query : Madge_query.t }

type 'resource route =
  { request_to_resource : request -> 'resource option ;
    resource_to_request : 'resource -> request option }

(** {2 Builders} *)

(** FIXME: Those builders are disgusting. There is a nice DSL for building
    queries hiding somewhere behind. *)

let direct method_ path resource =
  let request_to_resource req' =
    if req'.method_ = method_ && req'.path = path
    then Some resource
    else None
  in
  let resource_to_request resource' =
    if resource = resource'
    then Some { method_; path; query=Madge_query.empty }
    else None
  in
  { request_to_resource; resource_to_request }

let with_query method_ path makeResource unResource =
  let request_to_resource req' =
    if req'.method_ = method_ && req'.path = path
    then Option.some @@ makeResource req'.query
    else None
  in
  let resource_to_request resource' =
    Option.map (fun query -> { method_; path; query }) @@ unResource resource'
  in
  { request_to_resource; resource_to_request }

let with_slug_and_query method_ prefix ?ext makeResource unResource =
  let request_to_resource req' =
    if req'.method_ = method_ then
      (
        match String.rindex_opt req'.path '/' with
        | None -> None
        | Some i ->
          let prefix' = String.sub req'.path 0 i in
          if prefix' = prefix then
            (
              let suffix' = String.sub req'.path (i+1) (String.length req'.path - i-1) in
              match ext with
              | None -> Option.some @@ makeResource (Slug.unsafe_of_string suffix') req'.query
              | Some ext ->
                let ext = "." ^ ext in
                if Filename.check_suffix suffix' ext
                then Option.some @@ makeResource (Slug.unsafe_of_string @@ Filename.chop_suffix suffix' ext) req'.query
                else None
            )
          else
            None
      )
    else
      None
  in
  let resource_to_request resource =
    match unResource resource with
    | None -> None
    | Some (slug, query) ->
      let slug = Slug.to_string slug in
      Some {
        method_ ;
        path = prefix ^ "/" ^ slug ^ (match ext with None -> "" | Some ext -> "." ^ ext) ;
        query ;
      }
  in
  { request_to_resource; resource_to_request }

let with_slug method_ prefix ?ext (makeResource, unResource) =
  with_slug_and_query method_ prefix ?ext
    (fun slug _query -> makeResource slug)
    (fun resource -> Option.map (fun slug -> (slug, Madge_query.empty)) (unResource resource))

(** {2 Matchers} *)

let request_to_resource request routes =
  routes
  |> List.map (fun { request_to_resource; _ } -> request_to_resource request)
  |> List.find_opt ((<>) None)
  >>=? fun x -> x

let resource_to_request resource routes =
  let first_matching_route =
    routes
    |> List.map (fun { resource_to_request; _ } -> resource_to_request resource)
    |> List.find_opt ((<>) None)
  in
  match first_matching_route with
  | None -> failwith "Madge_router.resource_to_request"
  | Some None -> assert false
  | Some (Some request) -> request
