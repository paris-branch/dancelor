open Dancelor_common open Option
open Protocol_conv_jsonm

type version =
  { subslug : Slug.t option ;
    name : string option ;
    bars : int ;
    key : Music.key ;
    structure : string ;
    arranger : Credit.t option ;
    content : string }
[@@deriving to_protocol ~driver:(module Jsonm)]

type t =
  { slug : Slug.t ;
    name : string ;
    kind : Kind.base ;
    author : Credit.t option ;
    remark : string ;
    default_version : version ;
    other_versions : version list }
[@@deriving to_protocol ~driver:(module Jsonm)]

let version_add_tune_slug_and_type ~tune_slug =
  Json.on_value @@
    Json.add_fields
      [ "type", `String "tune-version" ;
        "tune-slug", `String tune_slug ]

let to_jsonm tune =
  to_jsonm tune
  |> Json.of_value
  |> Json.add_field "type" (`String "tune")
  |> Json.map_field "default_version" (version_add_tune_slug_and_type ~tune_slug:tune.slug)
  |> Json.map_field "other_versions" (function
         | `A versions -> `A (List.map (version_add_tune_slug_and_type ~tune_slug:tune.slug) versions)
         | _ -> assert false)
  |> Json.to_value

let to_json = to_jsonm ||> Json.of_value

(* FIXME: add type to versions *)

let version_to_jsonm ~tune_slug =
  version_to_jsonm ||> version_add_tune_slug_and_type ~tune_slug

type tune_version = t * version

let tune_version_to_jsonm (tune, version) =
  `O [
      "tune", to_jsonm tune;
      "version", version_to_jsonm ~tune_slug:tune.slug version
    ]

let tune_version_to_json = tune_version_to_jsonm

let version_unserialize (json, content) =
  let tune =
    { subslug = Json.(get_opt ~k:slug ["subslug"] json) ;
      name = Json.(get_opt ~k:string ["name"] json) ;
      bars = Json.(get ~k:int ["bars"] json) ;
      key = Json.(get ~k:(string >=> (Music.key_of_string ||> wrap)) ["key"] json) ;
      structure = Json.(get ~k:string ["structure"] json) ;
      arranger = Json.(get_opt ~k:(slug >=> (Credit.Database.get ||> wrap)) ["arranger"] json) ;
      content }
  in
  tune

let unserialize (json, versions) =
  let author = Json.(get_opt ~k:(slug >=> (Credit.Database.get ||> wrap)) ["author"] json) in
  let default_version, other_versions =
    match Json.(get_opt ~k:slug ["default"] json) with
    | Some subslug ->
       (match List.partition (fun version -> version.subslug = Some subslug) versions with
        | [default_version], other_versions -> default_version, other_versions
        | _ -> failwith "Dancelor_model.Tune.unserialize: several versions with the same subslug")
    | None ->
       (match versions with
        | [default_version] -> default_version, []
        | _ -> failwith "Dancelor_model.Tune.unserialize: several versions but no default field")
  in
  { slug = Json.(get ~k:slug ["slug"] json) ;
    name = Json.(get ~k:string ["name"] json) ;
    kind = Json.(get ~k:(string >=> (Kind.base_of_string ||> wrap)) ["kind"] json) ;
    remark = Json.(get_or ~k:string ~default:"" ["remark"] json) ;
    author ; default_version ; other_versions }

let version_subslug v = v.subslug
let version_name v = (v : version).name
let version_key v = v.key
let version_content v = v.content

let slug t = t.slug
let name t = t.name

let default_version t = t.default_version
let versions t = t.default_version :: t.other_versions
let version t subslug =
  List.find (fun version -> version_subslug version = Some subslug) (versions t)

let version_name t v =
  match version_name v with
  | None -> name t
  | Some name -> name

let match_score needle haystack =
  1. -.
    if String.length needle = 0 then
      0.
    else
      let d = String.inclusion_distance needle haystack in
      (float_of_int d) /. (float_of_int (String.length needle))

module Database =
  struct
    let prefix = "tune"

    let db = Hashtbl.create 8

    let initialise () =
      let read_version entry subentry =
        let json = Storage.read_subentry_json prefix entry subentry "meta.json" in
        let content = Storage.read_subentry_file prefix entry subentry "content.ly" in
        version_unserialize (json, content)
      in
      let load entry =
        let json = Storage.read_entry_json prefix entry "meta.json" in
        let versions =
          Storage.list_subentries prefix entry
          |> List.map (read_version entry)
        in
        let tune = unserialize (json, versions) in
        Hashtbl.add db tune.slug tune
      in
      Storage.list_entries prefix
      |> List.iter load

    (* let find_uniq_slug string =
     *   let slug = Slug.from_string string in
     *   let rec aux i =
     *     let slug = slug ^ "-" ^ (string_of_int i) in
     *     if Hashtbl.mem db slug then
     *       aux (i+1)
     *     else
     *       slug
     *   in
     *   if Hashtbl.mem db slug then
     *     aux 2
     *   else
     *     slug *)

    let get = Hashtbl.find db

    let get_all ?name ?author ?kind ?keys ?mode () =
      ignore keys; ignore mode;
      Hashtbl.to_seq_values db
      |> Seq.flat_map
           (fun tune ->
             versions tune
             |> List.map (fun version -> (1., tune, version))
             |> List.to_seq)
      |> Seq.map
           (match name with
            | None -> fun x -> x
            | Some name ->
               (fun (score, tune, version) ->
                 (score *. match_score name (version_name tune version), tune, version)))
      |> Seq.map
           (match author with
            | None -> fun x -> x
            | Some author ->
               (fun (score, tune, version) ->
                 match tune.author with
                 | None -> (0., tune, version)
                 | Some author' ->
                    (score *. match_score author (Credit.line author'), tune, version)))
      |> Seq.filter
           (match kind with
            | None -> fun _ -> true
            | Some kind -> (fun (_, tune, _) -> tune.kind = kind))
      |> Seq.filter
           (match keys with
            | None -> fun _ -> true
            | Some keys -> (fun (_, _, version) -> List.mem (version_key version) keys))
      |> Seq.filter
           (match mode with
            | None -> fun _ -> true
            | Some mode -> (fun (_, _, version) -> snd (version_key version) = mode))
      |> List.of_seq
      |> List.sort
           (fun (s1, t1, v1) (s2, t2, v2) ->
             let c = - compare s1 s2 in (* Scores in decreasing order *)
             if c = 0 then
               let c' = compare (slug t1) (slug t2) in
               if c' = 0 then
                 compare (version_subslug v1) (version_subslug v2)
               else
                 c'
             else
               c)
  end
