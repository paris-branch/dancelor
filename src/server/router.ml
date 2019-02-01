open Dancelor_common open Option
open Dancelor_model
module Log = (val Log.create "dancelor.server.router")

type controller =
  | Index
  | Credit of Credit.t
  | Pascaline
  | Person of Person.t
  | ProgramAll
  | ProgramPdf of Program.t
  | Program of Program.t
  | SetAll
  | SetCompose
  | SetSave
  | SetLy of Set.t
  | SetPdf of Set.t
  | Set of Set.t
  | TuneGroup of TuneGroup.t
  | TuneAll
  | TuneLy of Tune.t
  | TunePng of Tune.t
  | Tune of Tune.t
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
    controller_to_slug >=> fun slug ->
    Some (meth_to_match, prefix ^ "/" ^ slug ^ (match ext with None -> "" | Some ext -> "." ^ ext))
  )

let routes : route list =
  [
    direct
      ~meth:`GET
      ~path:"/"
      Index ;

    with_slug
      ~meth:`GET
      ~prefix:"/credit"
      (Credit.Database.get_opt >=> fun credit -> Some (Credit credit))
      (function Credit credit -> Some (Credit.slug credit)
              | _ -> None) ;

    direct
      ~meth:`GET
      ~path:"/pascaline"
      Pascaline ;

    with_slug
      ~meth:`GET
      ~prefix:"/person"
      (Person.Database.get_opt >=> fun person -> Some (Person person))
      (function Person person -> Some (Person.slug person)
              | _ -> None) ;

    direct
      ~meth:`GET
      ~path:"/program/all"
      ProgramAll ;

    with_slug
      ~meth:`GET
      ~prefix:"/program"
      ~ext:"pdf"
      (Program.Database.get_opt >=> fun program -> Some (ProgramPdf program))
      (function ProgramPdf program -> Some (Program.slug program)
              | _ -> None) ;

    with_slug
      ~meth:`GET
      ~prefix:"/program"
      (Program.Database.get_opt >=> fun program -> Some (Program program))
      (function Program program -> Some (Program.slug program)
              | _ -> None) ;

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
      (Set.Database.get_opt >=> fun set -> Some (SetLy set))
      (function SetLy set -> Some (Set.slug set)
              | _ -> None) ;

    with_slug
      ~meth:`GET
      ~prefix:"/set"
      ~ext:"pdf"
      (Set.Database.get_opt >=> fun set -> Some (SetPdf set))
      (function SetPdf set -> Some (Set.slug set)
              | _ -> None) ;

    with_slug
      ~meth:`GET
      ~prefix:"/set"
      (Set.Database.get_opt >=> fun set -> Some (Set set))
      (function Set set -> Some (Set.slug set)
              | _ -> None) ;

    with_slug
      ~meth:`GET
      ~prefix:"/tune-group"
      (TuneGroup.Database.get_opt >=> fun tune_group -> Some (TuneGroup tune_group))
      (function TuneGroup tune_group -> Some (TuneGroup.slug tune_group)
              | _ -> None) ;

    direct
      ~meth:`GET
      ~path:"/tune/all"
      TuneAll ;

    with_slug
      ~meth:`GET
      ~prefix:"/tune"
      ~ext:"ly"
      (Tune.Database.get_opt >=> fun tune -> Some (TuneLy tune))
      (function TuneLy tune -> Some (Tune.slug tune)
              | _ -> None) ;

    with_slug
      ~meth:`GET
      ~prefix:"/tune"
      ~ext:"png"
      (Tune.Database.get_opt >=> fun tune -> Some (TunePng tune))
      (function TunePng tune -> Some (Tune.slug tune)
              | _ -> None) ;

    with_slug
      ~meth:`GET
      ~prefix:"/tune"
      (Tune.Database.get_opt >=> fun tune -> Some (Tune tune))
      (function Tune tune -> Some (Tune.slug tune)
              | _ -> None) ;

    direct
      ~meth:`GET
      ~path:"/victor"
      Victor ;
  ]

let path_to_controller ~meth ~path =
  routes
  |> List.map (fun (path_to_controller, _) -> path_to_controller ~meth ~path)
  |> List.find_opt ((<>) None)
  >>= fun x -> x

let path_of_controller controller =
  routes
  |> List.map (fun (_, path_of_controller) -> path_of_controller controller)
  |> List.find_opt ((<>) None)
  >>= fun x -> x
