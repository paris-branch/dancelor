open Nes
open Dancelor_common_model

(** Existing resources in Dancelor. *)
type resource =
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

(** Constructors that can be used as functions. FIXME: This is a job for a PPX
    and there is probably one that exists for that. *)
let credit slug = Credit slug
let dance slug = Dance slug
let person slug = Person slug
let bookPdf slug = BookPdf slug
let book slug = Book slug
let setLy slug = SetLy slug
let setPdf slug = SetPdf slug
let set slug = Set slug
let setDelete slug = SetDelete slug
let tune slug = Tune slug
let versionLy slug = VersionLy slug
let versionSvg slug = VersionSvg slug
let versionOgg slug = VersionOgg slug
let versionPdf slug = VersionPdf slug
let version slug = Version slug

(** Destructors. FIXME: This is also a job for a PPX. *)
let unCredit = function Credit slug -> Some slug | _ -> None
let unDance = function Dance slug -> Some slug | _ -> None
let unPerson = function Person slug -> Some slug | _ -> None
let unBookPdf = function BookPdf slug -> Some slug | _ -> None
let unBook = function Book slug -> Some slug | _ -> None
let unSetLy = function SetLy slug -> Some slug | _ -> None
let unSetPdf = function SetPdf slug -> Some slug | _ -> None
let unSet = function Set slug -> Some slug | _ -> None
let unSetDelete = function SetDelete slug -> Some slug | _ -> None
let unTune = function Tune slug -> Some slug | _ -> None
let unVersionLy = function VersionLy slug -> Some slug | _ -> None
let unVersionSvg = function VersionSvg slug -> Some slug | _ -> None
let unVersionOgg = function VersionOgg slug -> Some slug | _ -> None
let unVersionPdf = function VersionPdf slug -> Some slug | _ -> None
let unVersion = function Version slug -> Some slug | _ -> None

(** {2 Routes}

    This part of the code defines a notion of {!route} linking a path and a
    resource. It also defines two ways to build such routes. *)

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

let routes : route list =
  [
    direct      `GET  "/"                    Index ;
    direct      `GET  "/search"              MagicSearch ;
    direct      `GET  "/credit/save"         CreditSave ;
    with_slug   `GET  "/credit"             (credit, unCredit) ;
    with_slug   `GET  "/dance"              (dance, unDance) ;
    direct      `GET  "/person/save"         PersonSave ;
    with_slug   `GET  "/person"             (person, unPerson) ;
    direct      `GET  "/book/all"            BookAll ;
    direct      `GET  "/book/compose"        BookCompose ;
    with_slug   `GET  "/book" ~ext:"pdf"    (bookPdf, unBookPdf) ;
    with_slug   `GET  "/book"               (book, unBook) ;
    direct      `GET  "/set/all"             SetAll ;
    direct      `GET  "/set/compose"         SetCompose ;
    direct      `GET  "/set/save"            SetSave ;
    with_slug   `GET  "/set" ~ext:"ly"      (setLy, unSetLy) ;
    with_slug   `GET  "/set" ~ext:"pdf"     (setPdf, unSetPdf) ;
    with_slug   `GET  "/set"                (set, unSet) ;
    with_slug `DELETE "/set"                (setDelete, unSetDelete) ;
    with_slug   `GET  "/tune"               (tune, unTune) ;
    direct      `GET  "/version/add"         VersionAddition ;
    direct      `GET  "/version/all"         VersionAll ;
    direct      `GET  "/version/search"      VersionSearch ;
    with_slug   `GET  "/version" ~ext:"ly"  (versionLy, unVersionLy) ;
    with_slug   `GET  "/version" ~ext:"svg" (versionSvg, unVersionSvg) ;
    with_slug   `GET  "/version" ~ext:"ogg" (versionOgg, unVersionOgg) ;
    with_slug   `GET  "/version" ~ext:"pdf" (versionPdf, unVersionPdf) ;
    with_slug   `GET  "/version"            (version,    unVersion) ;
    direct      `GET  "/victor"              Victor ;
  ]

let path_to_resource ~meth ~path =
  routes
  |> List.map (fun { path_to_resource; _ } -> path_to_resource meth path)
  |> List.find_opt ((<>) None)
  >>=? fun x -> x

let path_of_resource ~api_prefix resource =
  let first_matching_route =
    routes
    |> List.map (fun { resource_to_path; _ } -> resource_to_path resource)
    |> List.find_opt ((<>) None)
  in
  match first_matching_route with
  | None -> failwith "path_of_resource"
  | Some None -> assert false
  | Some (Some (meth, path)) ->
    let path =
      if api_prefix
      then Constant.api_prefix ^ "/" ^ path
      else path
    in
    (meth, path)

let path_of_get_resource ~api_prefix resource =
  let (meth, path) = path_of_resource ~api_prefix resource in
  if not (meth = `GET) then
    failwith "path_of_get_resource";
  path

let gpath ~api resource =
  path_of_get_resource ~api_prefix:api resource
