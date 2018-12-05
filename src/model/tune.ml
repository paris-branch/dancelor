open Dancelor_common
open Protocol_conv_jsonm
open Protocol_conv_yaml

type t =
  { slug : Slug.t ;
    name : string ;
    kind : Kind.tune ;
    author : Slug.t ;
    content : string }
[@@deriving protocol ~driver:(module Jsonm),
            protocol ~driver:(module Yaml)]

let match_score needle haystack =
  Format.eprintf "match_score \"%s\" \"%s\"@." needle haystack;
  let r =
    1. -.
      if String.length needle = 0 then
        0.
      else
        let d = String.inclusion_distance needle haystack in
        (float_of_int d) /. (float_of_int (String.length needle))
  in
  Format.eprintf "--> %f@." r;
  r

module Database =
  struct
    let prefix = "tune"

    let db = Hashtbl.create 8

    let initialise () =
      Storage.list_entries prefix
      |> List.iter
           (fun slug ->
             Storage.read_yaml prefix slug "meta.yaml"
             |> of_yaml
             |> Hashtbl.add db slug)

    let get = Hashtbl.find db

    let get_all ?(name="") ?(author="") ?kind () =
      Format.eprintf "get_all:@\n  name = %s@\n  author = %s@." name author;
      Hashtbl.to_seq_values db
      |> List.of_seq
      |> List.map (fun tune -> (1., tune))
      |> List.map (fun (score, tune) ->
             (score *. match_score (Slug.from_string name) tune.name, tune))
      |> List.map (fun (score, tune) ->
             (score *. match_score (Slug.from_string author) Credit.(Database.get tune.author |> line |> Slug.from_string), tune))
      |> List.filter
           (match kind with
            | None -> fun _ -> true
            | Some kind -> (fun (_, tune) -> snd tune.kind = kind))
      |> List.sort
           (fun (s1, t1) (s2, t2) ->
             let c = compare s1 s2 in
             if c = 0 then
               compare t1.slug t2.slug
             else
               c)
  end

type view =
  { slug : Slug.t ;
    name : string ;
    author : Credit.view ;
    kind : Kind.tune ;
    content : string }
[@@deriving protocol ~driver:(module Jsonm)]

let view (tune : t) =
  { slug = tune.slug ;
    name = tune.name ;
    author = Credit.(Database.get ||> view) tune.author ;
    kind = tune.kind ;
    content = tune.content }
