open Dancelor_common
open Protocol_conv_jsonm
open Protocol_conv_yaml

type t =
  { slug : Slug.t ;
    name : string ;
    kind : Kind.dance ;
    tunes : Slug.t list }
[@@deriving protocol ~driver:(module Jsonm),
            protocol ~driver:(module Yaml)]

module Database =
  struct
    let prefix = "set"

    let db = Hashtbl.create 8

    let initialise =
      let initialised = ref false in
      fun () ->
      Format.eprintf "sdojfnsdo@.";
      if not !initialised then
        (
          Storage.list_entries prefix
          |> List.iter
               (fun slug ->
                 Format.eprintf "- %s@." slug;
                 Storage.read_yaml prefix slug "meta.yaml"
                 |> of_yaml
                 |> Hashtbl.add db slug);
          initialised := true
        )

    let get = Hashtbl.find db
  end

type view =
  { slug : Slug.t ;
    name : string ;
    kind : Kind.dance ;
    tunes : Tune.view list }
[@@deriving protocol ~driver:(module Jsonm)]

let view ({ slug ; name ; kind ; tunes } : t) =
  { slug ; name ; kind ;
    tunes = List.map Tune.(Database.get ||> view) tunes }
