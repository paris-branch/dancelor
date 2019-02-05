open Dancelor_common open Option
open Protocol_conv_jsonm

type t =
  { slug : Slug.t ;
    group : TuneGroup.t ;
    bars : int ;
    key : Music.key ;
    structure : string ;
    arranger : Credit.t option ;
    content : string }
[@@deriving protocol ~driver:(module Jsonm)]

let to_jsonm tune =
  to_jsonm tune
  |> Json.of_value
  |> Json.add_field "type" (`String "tune")
  |> Json.to_value

let to_json = to_jsonm ||> Json.of_value

let of_json = Json.to_value ||> of_jsonm

let unserialize (json, content) =
  { slug = Json.(get ~k:slug ["slug"] json) ;
    group = Json.(get ~k:(slug >=> (TuneGroup.Database.get ||> wrap)) ["tune-group"] json) ;
    bars = Json.(get ~k:int ["bars"] json) ;
    key = Json.(get ~k:(string >=> (Music.key_of_string ||> wrap)) ["key"] json) ;
    structure = Json.(get ~k:string ["structure"] json) ;
    arranger = Json.(get_opt ~k:(slug >=> (Credit.Database.get ||> wrap)) ["arranger"] json) ;
    content }

let slug tune = tune.slug
let group tune = tune.group
let key tune = tune.key
let content tune = tune.content
let bars tune = tune.bars
let structure tune = tune.structure

module Database =
  struct
    let prefix = "tune"

    let db = Hashtbl.create 8

    let initialise () =
      let load entry =
        let json = Storage.read_entry_json prefix entry "meta.json" in
        let content = Storage.read_entry_file prefix entry "content.ly" in
        let tune = unserialize (json, content) in
        Hashtbl.add db tune.slug tune
      in
      Storage.list_entries prefix
      |> List.iter load

    let get = Hashtbl.find db
    let get_opt = Hashtbl.find_opt db

    let match_score needle haystack =
      let needle = Slug.from_string needle in
      let haystack = Slug.from_string haystack in
      1. -.
        if String.length needle = 0 then
          0.
        else
          let d = String.inclusion_distance needle haystack in
          (float_of_int d) /. (float_of_int (String.length needle))

    let get_all ?name ?author ?kind ?keys ?mode ?(hard_limit=max_int) ?(threshold=0.) () =
      ignore keys; ignore mode;
      Hashtbl.to_seq_values db
      |> Seq.map (fun tune -> (1., tune))
      |> Seq.map
           (match name with
            | None -> fun x -> x
            | Some name ->
               (fun (score, tune) ->
                 (score *. match_score name (TuneGroup.name (group tune)), tune)))
      |> Seq.map
           (match author with
            | None -> fun x -> x
            | Some author ->
               (fun (score, tune) ->
                 match TuneGroup.author (group tune) with
                 | None -> (0., tune)
                 | Some author' ->
                    (score *. match_score author (Credit.line author'), tune)))
      |> Seq.filter
           (match kind with
            | None -> fun _ -> true
            | Some kind -> (fun (_, tune) -> TuneGroup.kind (group tune) = kind))
      |> Seq.filter
           (match keys with
            | None -> fun _ -> true
            | Some keys -> (fun (_, tune) -> List.mem (key tune) keys))
      |> Seq.filter
           (match mode with
            | None -> fun _ -> true
            | Some mode -> (fun (_, tune) -> snd (key tune) = mode))
      |> Seq.filter
           (fun (score, _) -> score >= threshold)
      |> Seq.sub hard_limit
      |> List.of_seq
      |> List.sort
           (fun (score1, tune1) (score2, tune2) ->
             let c = - compare score1 score2 in (* Scores in decreasing order *)
             if c = 0 then
               compare (slug tune1) (slug tune2)
             else
               c)
  end
