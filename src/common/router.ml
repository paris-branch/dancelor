open Nes
open Dancelor_common_model

type resource = (** FIXME: resource = endpoint U page *)
  | Index
  | MagicSearch (* FIXME: argument *)

  | CreditSave
  | Credit of CreditCore.t Slug.t

  | Dance of DanceCore.t Slug.t

  | PersonSave
  | Person of PersonCore.t Slug.t

  | BookAll
  | BookCompose
  | BookPdf of BookCore.t Slug.t
  | Book of BookCore.t Slug.t

  | SetAll
  | SetCompose
  | SetSave
  | SetLy of SetCore.t Slug.t
  | SetPdf of SetCore.t Slug.t
  | Set of SetCore.t Slug.t
  | SetDelete of SetCore.t Slug.t

  | Tune of TuneCore.t Slug.t

  | VersionAddition
  | VersionAll
  | VersionSearch
  | VersionLy of VersionCore.t Slug.t
  | VersionSvg of VersionCore.t Slug.t
  | VersionOgg of VersionCore.t Slug.t
  | VersionPdf of VersionCore.t Slug.t
  | Version of VersionCore.t Slug.t

  | Victor

type route =
  { path_to_resource : Cohttp.Code.meth -> string -> resource option ;
    resource_to_path : resource -> (Cohttp.Code.meth * string) option }

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

let with_slug ~meth ~prefix ?ext slug_to_controller controller_to_slug =
  let meth_to_match = meth in
  let prefix_to_match = prefix in
  (
    fun ~meth ~path ->
      if meth = meth_to_match then
        (
          match String.rindex_opt path '/' with
          | None -> None
          | Some i ->
            let prefix = String.sub path 0 i in
            if prefix = prefix_to_match then
              (
                let suffix = String.sub path (i+1) (String.length path - i-1) in
                match ext with
                | None -> slug_to_controller (Slug.unsafe_of_string suffix)
                | Some ext ->
                  let ext = "." ^ ext in
                  if Filename.check_suffix suffix ext then
                    (
                      slug_to_controller (Slug.unsafe_of_string (Filename.chop_suffix suffix ext))
                    )
                  else
                    None
              )
            else
              None
        )
      else
        None
  ),
  (
    controller_to_slug >=>? fun slug ->
      let slug = Slug.to_string slug in
      Some (meth_to_match, prefix ^ "/" ^ slug ^ (match ext with None -> "" | Some ext -> "." ^ ext))
  )

let routes : route list =
  [
    direct `GET "/" Index ;
    direct `GET "/search" MagicSearch ;
    direct `GET "/credit/save" CreditSave ;

    with_slug
      ~meth:`GET
      ~prefix:"/credit"
      (fun credit -> Some (Credit credit))
      (function Credit credit -> Some credit | _ -> None) ;

    with_slug
      ~meth:`GET
      ~prefix:"/dance"
      (fun dance -> Some (Dance dance))
      (function Dance dance -> Some dance | _ -> None) ;

    direct `GET "/person/save" PersonSave ;

    with_slug
      ~meth:`GET
      ~prefix:"/person"
      (fun person -> Some (Person person))
      (function Person person -> Some person | _ -> None) ;

    direct `GET "/book/all" BookAll ;
    direct `GET "/book/compose" BookCompose ;

    with_slug
      ~meth:`GET
      ~prefix:"/book"
      ~ext:"pdf"
      (fun book -> Some (BookPdf book))
      (function BookPdf book -> Some book | _ -> None) ;

    with_slug
      ~meth:`GET
      ~prefix:"/book"
      (fun book -> Some (Book book))
      (function Book book -> Some book | _ -> None) ;

    direct `GET "/set/all" SetAll ;
    direct `GET "/set/compose" SetCompose ;
    direct `GET "/set/save" SetSave ;

    with_slug
      ~meth:`GET
      ~prefix:"/set"
      ~ext:"ly"
      (fun set -> Some (SetLy set))
      (function SetLy set -> Some set | _ -> None) ;

    with_slug
      ~meth:`GET
      ~prefix:"/set"
      ~ext:"pdf"
      (fun set -> Some (SetPdf set))
      (function SetPdf set -> Some set | _ -> None) ;

    with_slug
      ~meth:`GET
      ~prefix:"/set"
      (fun set -> Some (Set set))
      (function Set set -> Some set | _ -> None) ;

    with_slug
      ~meth:`DELETE
      ~prefix:"/set"
      (fun set -> Some (SetDelete set))
      (function SetDelete set -> Some set | _ -> None) ;

    with_slug
      ~meth:`GET
      ~prefix:"/tune"
      (fun tune -> Some (Tune tune))
      (function Tune tune -> Some tune | _ -> None) ;

    direct `GET "/version/add" VersionAddition ;
    direct `GET "/version/all" VersionAll ;
    direct `GET "/version/search" VersionSearch ;

    with_slug
      ~meth:`GET
      ~prefix:"/version"
      ~ext:"ly"
      (fun version -> Some (VersionLy version))
      (function VersionLy version -> Some version | _ -> None) ;

    with_slug
      ~meth:`GET
      ~prefix:"/version"
      ~ext:"svg"
      (fun version -> Some (VersionSvg version))
      (function VersionSvg version -> Some version | _ -> None) ;

    with_slug
      ~meth:`GET
      ~prefix:"/version"
      ~ext:"ogg"
      (fun version -> Some (VersionOgg version))
      (function VersionOgg version -> Some version | _ -> None) ;

    with_slug
      ~meth:`GET
      ~prefix:"/version"
      ~ext:"pdf"
      (fun version -> Some (VersionPdf version))
      (function VersionPdf version -> Some version | _ -> None) ;

    with_slug
      ~meth:`GET
      ~prefix:"/version"
      (fun version -> Some (Version version))
      (function Version version -> Some version | _ -> None) ;

    direct `GET "/victor" Victor ;
  ]

let path_to_controller ~meth ~path =
  routes
  |> List.map (fun (path_to_controller, _) -> path_to_controller ~meth ~path)
  |> List.find_opt ((<>) None)
  >>=? fun x -> x

let path_of_controller ~api_prefix controller =
  let first_matching_route =
    routes
    |> List.map (fun (_, path_of_controller) -> path_of_controller controller)
    |> List.find_opt ((<>) None)
  in
  match first_matching_route with
  | None -> failwith "path_of_controller"
  | Some None -> assert false
  | Some (Some (meth, path)) ->
    let path =
      if api_prefix
      then Constant.api_prefix ^ "/" ^ path
      else path
    in
    (meth, path)

let path_of_get_controller ~api_prefix controller =
  let (meth, path) = path_of_controller ~api_prefix controller in
  if not (meth = `GET) then
    failwith "path_of_get_controller";
  path

let gpath ~api controller =
  path_of_get_controller ~api_prefix:api controller
