open Nes
open Dancelor_common_model

type (_, _, _) endpoint =
  | Person : ('a, 'w, 'r) PersonEndpoints.t -> ('a, 'w, 'r) endpoint
  | Book : ('a, 'w, 'r) BookEndpoints.t -> ('a, 'w, 'r) endpoint
  | Version : ('a, 'w, 'r) VersionEndpoints.t -> ('a, 'w, 'r) endpoint
  | Dance : ('a, 'w, 'r) DanceEndpoints.t -> ('a, 'w, 'r) endpoint
  | Set : ('a, 'w, 'r) SetEndpoints.t -> ('a, 'w, 'r) endpoint
  | Tune : ('a, 'w, 'r) TuneEndpoints.t -> ('a, 'w, 'r) endpoint
  | Victor : ('w, 'w, Void.t) endpoint

type endpoint_wrapped =
  | W : ('a, 'r Lwt.t, 'r) endpoint -> endpoint_wrapped

let all_endpoints =
  List.flatten
    [
      List.map (fun (PersonEndpoints.W e) -> W (Person e)) PersonEndpoints.all;
      List.map (fun (BookEndpoints.W e) -> W (Book e)) BookEndpoints.all;
      List.map (fun (VersionEndpoints.W e) -> W (Version e)) VersionEndpoints.all;
      List.map (fun (DanceEndpoints.W e) -> W (Dance e)) DanceEndpoints.all;
      List.map (fun (SetEndpoints.W e) -> W (Set e)) SetEndpoints.all;
      List.map (fun (TuneEndpoints.W e) -> W (Tune e)) TuneEndpoints.all;
      [W Victor];
    ]

open Madge

(* FIXME: Factorise adding the `/api` prefix. *)
let route : type a w r. (a, w, r) endpoint -> (a, w, r) route = function
  | Person endpoint -> literal "api" @@ literal "person" @@ PersonEndpoints.route endpoint
  | Book endpoint -> literal "api" @@ literal "book" @@ BookEndpoints.route endpoint
  | Version endpoint -> literal "api" @@ literal "version" @@ VersionEndpoints.route endpoint
  | Dance endpoint -> literal "api" @@ literal "dance" @@ DanceEndpoints.route endpoint
  | Set endpoint -> literal "api" @@ literal "set" @@ SetEndpoints.route endpoint
  | Tune endpoint -> literal "api" @@ literal "tune" @@ TuneEndpoints.route endpoint
  | Victor -> literal "api" @@ literal "victor" @@ return (module Void)

let path : type a r. (a, string, r) route -> a = fun route ->
  process route (fun (module _) uri -> Uri.to_string uri)
