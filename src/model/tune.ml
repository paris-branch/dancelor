open Dancelor_common
open Protocol_conv_jsonm

module Version = struct
  module Log = (val Log.create "dancelor.mode.tune.version" : Logs.LOG)

  type t =
    { subslug : Slug.t option ;
      name : string option ;
      bars : int ;
      key : Music.key ;
      arranger : Credit.t option ;
      content : string }
  [@@deriving to_protocol ~driver:(module Jsonm)]

  (* let serialize version =
   *   let subslug =
   *     match version.subslug with
   *     | None -> []
   *     | Some subslug -> ["subslug", `String subslug]
   *   in
   *   let name =
   *     match version.name with
   *     | None -> []
   *     | Some name -> ["name", `String name]
   *   in
   *   let arranger =
   *     match version.arranger with
   *     | None -> []
   *     | Some arranger -> ["arranger", `String (Credit.slug arranger)]
   *   in
   *   (`O ([
   *          "bars", `Float (float_of_int version.bars) ;
   *          "key", `String (Music.key_to_string version.key)
   *        ] @ subslug @ name @ arranger),
   *    version.content) *)

  let unserialize (json, content) =
    Log.debug (fun m -> m "Unserializing");
    let open Option in
    let open Ezjsonm in
    let open JsonHelpers in
    let tune =
      { subslug = (find_opt json ["subslug"] >>= fun v -> Some (get_slug v)) ;
        name = (find_opt json ["subslug"] >>= fun v -> Some (get_string v)) ;
        bars = get_int (find json ["bars"]) ;
        key = Music.key_of_string (get_string (find json ["key"])) ;
        arranger = (find_opt json ["arranger"] >>= fun v -> Some (Credit.Database.get (get_slug v))) ;
        content }
    in
    Log.debug (fun m -> m "Unserialization successful");
    tune

  let subslug v = v.subslug
  let name v = v.name
  let key v = v.key
  let content v = v.content
end

type t =
  { slug : Slug.t ;
    name : string ;
    kind : Kind.base ;
    author : Credit.t option ;
    remark : string ;
    default_version : Version.t ;
    other_versions : Version.t list }
[@@deriving to_protocol ~driver:(module Jsonm)]

(* let serialize tune =
 *   let author =
 *     match tune.author with
 *     | None -> []
 *     | Some author -> ["author", `String (Credit.slug author)]
 *   in
 *   let default =
 *     match Version.subslug tune.default_version with
 *     | None -> assert (tune.other_versions = []); []
 *     | Some subslug -> ["default", `String subslug]
 *   in
 *   (
 *     `O ([
 *           "tune", `String tune.slug ;
 *           "name", `String tune.name ;
 *           "kind", `String (Kind.base_to_string tune.kind) ;
 *           "remark", `String tune.remark ;
 *         ] @ author @ default),
 *     tune.default_version :: tune.other_versions
 *   ) *)

let unserialize (json, versions) =
  let open Option in
  let open Ezjsonm in
  let open JsonHelpers in
  let author =
    find_opt json ["author"] >>= fun value ->
    Some (Credit.Database.get (get_slug value))
  in
  let default_version, other_versions =
    try
      let subslug = get_slug (find json ["default"]) in
      (match List.partition (fun version -> Version.subslug version = Some subslug) versions with
       | [default_version], other_versions -> default_version, other_versions
       | _ -> failwith "Dancelor_model.Tune.unserialize: several versions with the same subslug")
    with
      Not_found ->
      (match versions with
       | [default_version] -> default_version, []
       | _ -> failwith "Dancelor_model.Tune.unserialize: several versions but no default field")
  in
  { slug = get_slug (find json ["slug"]) ;
    name = get_string (find json ["name"]) ;
    kind = Kind.base_of_string (get_string (find json ["kind"])) ;
    remark = ((find_opt json ["remark"] >>= fun v -> Some (get_string v)) |> Option.value ~default:"") ;
    author ; default_version ; other_versions }

let slug t = t.slug
let name t = t.name

let default_version t = t.default_version
let versions t = t.default_version :: t.other_versions
let version t subslug =
  List.find (fun version -> Version.subslug version = Some subslug) (versions t)

let version_name t v =
  match Version.name v with
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
        Version.unserialize (json, content)
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
            | Some keys -> (fun (_, _, version) -> List.mem (Version.key version) keys))
      |> Seq.filter
           (match mode with
            | None -> fun _ -> true
            | Some mode -> (fun (_, _, version) -> snd (Version.key version) = mode))
      |> List.of_seq
      |> List.sort
           (fun (s1, t1, v1) (s2, t2, v2) ->
             let c = - compare s1 s2 in (* Scores in decreasing order *)
             if c = 0 then
               let c' = compare (slug t1) (slug t2) in
               if c' = 0 then
                 compare (Version.subslug v1) (Version.subslug v2)
               else
                 c'
             else
               c)
  end
