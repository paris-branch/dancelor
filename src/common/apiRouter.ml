open Dancelor_common_model

type victor_level = One | Two | Three | Four

(** Existing endpoints in Dancelor's API. *)
type endpoint =
  | Victor of victor_level
[@@deriving variants]

(** {2 Routes} *)

open Madge_router
module MQ = Madge_query

let routes : endpoint route list = [
  direct `GET "/victor" @@ Victor One;
  direct `GET "/victor2" @@ Victor Two;
  direct `GET "/victor3" @@ Victor Three;
  direct `GET "/victor4" @@ Victor Four
]

let endpoint method_ path query =
  Madge_router.request_to_resource {method_; path; query} routes

(** NEW-NEW STYLE *)

type (_, _, _) endpoint_new =
  | Person : ('a, 'w, 'r) PersonEndpoints.t -> ('a, 'w, 'r) endpoint_new
  | Book : ('a, 'w, 'r) BookEndpoints.t -> ('a, 'w, 'r) endpoint_new
  | Version : ('a, 'w, 'r) VersionEndpoints.t -> ('a, 'w, 'r) endpoint_new
  | Dance : ('a, 'w, 'r) DanceEndpoints.t -> ('a, 'w, 'r) endpoint_new
  | Set : ('a, 'w, 'r) SetEndpoints.t -> ('a, 'w, 'r) endpoint_new
  | Tune : ('a, 'w, 'r) TuneEndpoints.t -> ('a, 'w, 'r) endpoint_new

type endpoint_new_wrapped =
  | W : ('a, 'r Lwt.t, 'r) endpoint_new -> endpoint_new_wrapped

let all_endpoints =
  List.flatten
    [
      List.map (fun (PersonEndpoints.W e) -> W (Person e)) PersonEndpoints.all;
      List.map (fun (BookEndpoints.W e) -> W (Book e)) BookEndpoints.all;
      List.map (fun (VersionEndpoints.W e) -> W (Version e)) VersionEndpoints.all;
      List.map (fun (DanceEndpoints.W e) -> W (Dance e)) DanceEndpoints.all;
      List.map (fun (SetEndpoints.W e) -> W (Set e)) SetEndpoints.all;
      List.map (fun (TuneEndpoints.W e) -> W (Tune e)) TuneEndpoints.all;
    ]

open Madge

(* FIXME: Factorise adding the `/api` prefix. *)
let route : type a w r. (a, w, r) endpoint_new -> (a, w, r) route = function
  | Person endpoint -> literal "api" @@ literal "person" @@ PersonEndpoints.route endpoint
  | Book endpoint -> literal "api" @@ literal "book" @@ BookEndpoints.route endpoint
  | Version endpoint -> literal "api" @@ literal "version" @@ VersionEndpoints.route endpoint
  | Dance endpoint -> literal "api" @@ literal "dance" @@ DanceEndpoints.route endpoint
  | Set endpoint -> literal "api" @@ literal "set" @@ SetEndpoints.route endpoint
  | Tune endpoint -> literal "api" @@ literal "tune" @@ TuneEndpoints.route endpoint

let path : type a r. (a, string, r) route -> a = fun route ->
  process route (fun (module _) uri -> Uri.to_string uri)
