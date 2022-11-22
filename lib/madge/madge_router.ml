(** {1 Madge Router} *)

open Nes

type 'resource route =
  { path_to_resource : Cohttp.Code.meth -> string -> 'resource option ;
    resource_to_path : 'resource -> (Cohttp.Code.meth * string) option }

(** {2 Builders} *)

let direct method_ path resource =
  let path_to_resource method_' path' =
    if method_' = method_ && path' = path
    then Some resource
    else None
  in
  let resource_to_path resource' =
    if resource = resource'
    then Some (method_, path)
    else None
  in
  { path_to_resource; resource_to_path }

let with_slug method_ prefix ?ext (makeResource, unResource) =
  let path_to_resource method_' path' =
    if method_' = method_ then
      (
        match String.rindex_opt path' '/' with
        | None -> None
        | Some i ->
          let prefix' = String.sub path' 0 i in
          if prefix' = prefix then
            (
              let suffix' = String.sub path' (i+1) (String.length path' - i-1) in
              match ext with
              | None -> Option.some @@ makeResource @@ Slug.unsafe_of_string suffix'
              | Some ext ->
                let ext = "." ^ ext in
                if Filename.check_suffix suffix' ext
                then Option.some @@ makeResource @@ Slug.unsafe_of_string @@ Filename.chop_suffix suffix' ext
                else None
            )
          else
            None
      )
    else
      None
  in
  let resource_to_path =
    unResource >=>? fun slug ->
      let slug = Slug.to_string slug in
      Some (method_, prefix ^ "/" ^ slug ^ (match ext with None -> "" | Some ext -> "." ^ ext))
  in
  { path_to_resource; resource_to_path }

 (** {2 Matchers} *)

let path_to_resource method_ path routes =
  routes
  |> List.map (fun { path_to_resource; _ } -> path_to_resource method_ path)
  |> List.find_opt ((<>) None)
  >>=? fun x -> x

let resource_to_path resource routes =
  let first_matching_route =
    routes
    |> List.map (fun { resource_to_path; _ } -> resource_to_path resource)
    |> List.find_opt ((<>) None)
  in
  match first_matching_route with
  | None -> failwith "resource_to_path"
  | Some None -> assert false
  | Some (Some (meth, path)) -> (meth, path)
