open Js_of_ocaml
open Nes
open Dancelor_common
open Model_new

module Log = (val Logs.src_log @@ Logs.Src.create "client.history": Logs.LOG)

let madge_call_or_option endpoint id =
  Lwt.flip_map (Madge_client.call (Endpoints.Api.route @@ endpoint) id) @@ function
    | Ok v -> Some v
    | Error (Madge_client.Http {status = `Not_found; _}) -> None
    | Error e -> raise (Madge_client.Error e)

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
let get_models () : Any_row.t list Lwt.t =
  Logger.bracket_lwt (module Log) "getting models" @@ fun () ->
  let model_val : type a r. (a, Any_row.t option Lwt.t option, r) Endpoints.Page.t -> a = function
    | Person -> (fun _ id -> Some (Option.map Any_row.person <$> madge_call_or_option (Person Get_row) id))
    | Dance -> (fun _ id -> Some (Option.map Any_row.dance <$> madge_call_or_option (Dance Get_row) id))
    | Source -> (fun _ id -> Some (Option.map Any_row.source <$> madge_call_or_option (Source Get_row) id))
    | Tune -> (fun _ id -> Some (Option.map Any_row.tune <$> madge_call_or_option (Tune Get_row) id))
    | Version -> (fun _ id -> Some (Option.map Any_row.version <$> madge_call_or_option (Version Get_row) id))
    | Set -> (fun _ id -> Some (Option.map Any_row.set <$> madge_call_or_option (Set Get_row) id))
    | Book -> (fun _ id -> Some (Option.map Any_row.book <$> madge_call_or_option (Book Get_row) id))
    (* FIXME: user once there is a user viewer page endpoint *)
    (* everything else we ignore *)
    | endpoint -> Endpoints.Page.consume endpoint ~return: None
  in
  let model_val uri : Any_row.t option Lwt.t option =
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
  let%lwt models = Lwt_list.filter_map_p Fun.id models in
  lwt @@ List.deduplicate ~eq: (Any_row.equal) models

(** Returns all the sets whose page is present in the history. *)
let get_sets () = List.filter_map (function Any_row.Set set -> Some set | _ -> None) <$> get_models ()

(** Returns all the books whose page is present in the history. *)
let get_books () = List.filter_map (function Any_row.Book book -> Some book | _ -> None) <$> get_models ()
