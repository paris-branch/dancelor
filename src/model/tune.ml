open Dancelor_common
open Protocol_conv_jsonm
open Protocol_conv_yaml

type t =
  { slug : Slug.t ;
    name : string ;
    disambiguation : string ;
    kind : Kind.tune ;
    key : Music.key ;
    author : Slug.t ;
    content : string }
[@@deriving protocol ~driver:(module Jsonm),
            protocol ~driver:(module Yaml)]

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
      Storage.list_entries prefix
      |> List.iter
           (fun slug ->
             Storage.read_yaml prefix slug "meta.yaml"
             |> of_yaml
             |> Hashtbl.add db slug)

    let find_uniq_slug string =
      let slug = Slug.from_string string in
      let rec aux i =
        let slug = slug ^ "-" ^ (string_of_int i) in
        if Hashtbl.mem db slug then
          aux (i+1)
        else
          slug
      in
      aux 0

    let get = Hashtbl.find db

    let get_all ?(name="") ?(author="") ?kind ?keys ?mode () =
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
      |> List.filter
           (match keys with
            | None -> fun _ -> true
            | Some keys -> (fun (_, tune) -> List.mem tune.key keys))
      |> List.filter
           (match mode with
            | None -> fun _ -> true
            | Some mode -> (fun (_, tune) -> snd tune.key = mode))
      |> List.sort
           (fun (s1, t1) (s2, t2) ->
             let c = - compare s1 s2 in
             if c = 0 then
               - compare t1.slug t2.slug
             else
               c)

    let create ~name ?(disambiguation="") ~kind ~key ~author ~content () =
      let slug = find_uniq_slug name in
      let tune =
        { slug ; name ; disambiguation ;
          kind ; key ;
          author = Credit.slug author ;
          content }
      in
      Hashtbl.add db slug tune;
      Storage.write_yaml prefix slug "meta.yaml" (to_yaml tune);
      (slug, tune)
  end

type view =
  { slug : Slug.t ;
    name : string ;
    disambiguation : string ;
    author : Credit.view ;
    kind : Kind.tune ;
    key : Music.key ;
    content : string }
[@@deriving protocol ~driver:(module Jsonm)]

let view (tune : t) =
  { slug = tune.slug ;
    name = tune.name ;
    disambiguation = tune.disambiguation ;
    author = Credit.(Database.get ||> view) tune.author ;
    kind = tune.kind ;
    key = tune.key ;
    content = tune.content }
