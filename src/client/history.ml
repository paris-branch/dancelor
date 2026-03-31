open Js_of_ocaml
open Nes
open Common

module Log = (val Logs.src_log @@ Logs.Src.create "client.history": Logs.LOG)

type history = (Datetime.t * Uri.t) list [@@deriving yojson]

let empty_history : history = []

(** Maximal size of the history. *)
let limit = 1000

let with_local_storage ~default f =
  Option.fold ~none: default ~some: f @@ Js.Optdef.to_option Dom_html.window##.localStorage

let get () =
  with_local_storage ~default: empty_history @@ fun local_storage ->
  match Js.Opt.to_option @@ local_storage##getItem (Js.string "history") with
  | None -> empty_history
  | Some history ->
    match history_of_yojson @@ Yojson.Safe.from_string @@ Js.to_string history with
    | Error _ -> empty_history
    | Ok history -> history

let set history =
  with_local_storage ~default: () @@ fun local_storage ->
  local_storage##setItem (Js.string "history") (Js.string @@ Yojson.Safe.to_string @@ history_to_yojson history)

let update f = set @@ f @@ get ()

let add (uri : Uri.t) : unit =
  update (fun history -> (Datetime.now (), uri) :: List.take (limit - 1) history)

(** Returns all the models whose page is present in the history. *)
let get_models () : Model.Any.t list Lwt.t =
  Logger.bracket_lwt (module Log) "getting models" @@ fun () ->
  let model_val : type a r. (a, Model.Any.t Lwt.t option, r) Endpoints.Page.t -> a = function
    | Person -> (fun _ id -> Some (Model.Any.person % Option.get <$> Model.Person.get id))
    | Dance -> (fun _ id -> Some (Model.Any.dance % Option.get <$> Model.Dance.get id))
    | Source -> (fun _ id -> Some (Model.Any.source % Option.get <$> Model.Source.get id))
    | Tune -> (fun _ id -> Some (Model.Any.tune % Option.get <$> Model.Tune.get id))
    | Version -> (fun _ _ id -> Some (Model.Any.version % Option.get <$> Model.Version.get id))
    | Set -> (fun _ id -> Some (Model.Any.set % Option.get <$> Model.Set.get id))
    | Book -> (fun _ id -> Some (Model.Any.book % Option.get <$> Model.Book.get id))
    (* FIXME: user once there is a user viewer page endpoint *)
    (* everything else we ignore *)
    | endpoint -> Endpoints.Page.consume endpoint ~return: None
  in
  let model_val uri : Model.Any.t Lwt.t option =
    Option.join @@
    Option.map (fun f -> f ()) @@
    List.find_map
      (fun (Endpoints.Page.W' endpoint) ->
        Madge.apply'
          (Endpoints.Page.route endpoint)
          (fun () -> model_val endpoint)
          (Madge.Request.make ~meth: GET ~uri ~body: "")
      )
      (Endpoints.Page.all' ())
  in
  let models = List.filter_map (model_val % snd) (get ()) in
  let%lwt models = Lwt_list.map_p Fun.id models in
  lwt @@ List.deduplicate ~eq: (Model.Any.equal) models

(** Returns all the sets whose page is present in the history. *)
let get_sets () = List.filter_map (function Model.Any.Set set -> Some set | _ -> None) <$> get_models ()

(** Returns all the books whose page is present in the history. *)
let get_books () = List.filter_map (function Model.Any.Book book -> Some book | _ -> None) <$> get_models ()
