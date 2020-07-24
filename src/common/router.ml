open Nes open Option.Syntax
open Dancelor_common_model

type controller =
  | Index

  | CreditSave
  | Credit of Credit.t Slug.t

  | Dance of Dance.t Slug.t

  | Pascaline

  | PersonSave
  | Person of Person.t Slug.t

  | ProgramAll
  | ProgramPdf of Program.t Slug.t
  | Program of Program.t Slug.t

  | SetAll
  | SetCompose
  | SetSave
  | SetLy of Set.t Slug.t
  | SetPdf of Set.t Slug.t
  | Set of Set.t Slug.t
  | SetDelete of Set.t Slug.t

  | TuneGroup of TuneGroup.t Slug.t

  | TuneAll
  | TuneSearch
  | TuneLy of Tune.t Slug.t
  | TuneSvg of Tune.t Slug.t
  | TunePdf of Tune.t Slug.t
  | Tune of Tune.t Slug.t

  | Victor

type route =
  (meth:Cohttp.Code.meth -> path:string -> controller option)
  * (controller -> (Cohttp.Code.meth * string) option)

let direct ~meth ~path controller =
  let meth_to_match = meth in
  let path_to_match = path in
  (
    fun ~meth ~path ->
    if meth = meth_to_match && path = path_to_match then
      Some controller
    else
      None
  ),
  (
    fun controller' ->
    if controller' = controller then
      Some (meth_to_match, path_to_match)
    else
      None
  )

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
               | None -> slug_to_controller suffix
               | Some ext ->
                  let ext = "." ^ ext in
                  if Filename.check_suffix suffix ext then
                    (
                      slug_to_controller (Filename.chop_suffix suffix ext)
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
    Some (meth_to_match, prefix ^ "/" ^ slug ^ (match ext with None -> "" | Some ext -> "." ^ ext))
  )

let routes : route list =
  [
    direct
      ~meth:`GET
      ~path:"/"
      Index ;

    direct
      ~meth:`GET
      ~path:"/credit/save"
      CreditSave ;

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

    direct
      ~meth:`GET
      ~path:"/pascaline"
      Pascaline ;

    direct
      ~meth:`GET
      ~path:"/person/save"
      PersonSave ;

    with_slug
      ~meth:`GET
      ~prefix:"/person"
      (fun person -> Some (Person person))
      (function Person person -> Some person | _ -> None) ;

    direct
      ~meth:`GET
      ~path:"/program/all"
      ProgramAll ;

    with_slug
      ~meth:`GET
      ~prefix:"/program"
      ~ext:"pdf"
      (fun program -> Some (ProgramPdf program))
      (function ProgramPdf program -> Some program | _ -> None) ;

    with_slug
      ~meth:`GET
      ~prefix:"/program"
      (fun program -> Some (Program program))
      (function Program program -> Some program | _ -> None) ;

    direct
      ~meth:`GET
      ~path:"/set/all"
      SetAll ;

    direct
      ~meth:`GET
      ~path:"/set/compose"
      SetCompose ;

    direct
      ~meth:`GET
      ~path:"/set/save"
      SetSave ;

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
      ~prefix:"/tune-group"
      (fun tune_group -> Some (TuneGroup tune_group))
      (function TuneGroup tune_group -> Some tune_group | _ -> None) ;

    direct
      ~meth:`GET
      ~path:"/tune/all"
      TuneAll ;

    direct
      ~meth:`GET
      ~path:"/tune/search"
      TuneSearch ;

    with_slug
      ~meth:`GET
      ~prefix:"/tune"
      ~ext:"ly"
      (fun tune -> Some (TuneLy tune))
      (function TuneLy tune -> Some tune | _ -> None) ;

    with_slug
      ~meth:`GET
      ~prefix:"/tune"
      ~ext:"svg"
      (fun tune -> Some (TuneSvg tune))
      (function TuneSvg tune -> Some tune | _ -> None) ;

    with_slug
      ~meth:`GET
      ~prefix:"/tune"
      ~ext:"pdf"
      (fun tune -> Some (TunePdf tune))
      (function TunePdf tune -> Some tune | _ -> None) ;

    with_slug
      ~meth:`GET
      ~prefix:"/tune"
      (fun tune -> Some (Tune tune))
      (function Tune tune -> Some tune | _ -> None) ;

    direct
      ~meth:`GET
      ~path:"/blaireau"
      Victor ;

    direct
      ~meth:`GET
      ~path:"/victor"
      Victor ;
  ]

let path_to_controller ~meth ~path =
  routes
  |> List.map (fun (path_to_controller, _) -> path_to_controller ~meth ~path)
  |> List.find_opt ((<>) None)
  >>=? fun x -> x

let path_of_controller controller =
  (
    routes
    |> List.map (fun (_, path_of_controller) -> path_of_controller controller)
    |> List.find_opt ((<>) None)
    >>=? fun x -> x
  )
  |> Option.unwrap
