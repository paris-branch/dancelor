open Js_of_ocaml
open Nes
open Common

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

(** Returns all the sets whose page is present in the history. *)
let get_sets () =
  let set_val : type a r. (a, Model.Set.t Entry.id option, r) Endpoints.Page.t -> a = function
    | Set -> (fun _ id -> Some id)
    (* everything else we ignore *)
    | endpoint -> Endpoints.Page.consume None endpoint
  in
  let set_val uri : Model.Set.t Entry.id option =
    Option.join @@
    Option.map (fun f -> f ()) @@
    List.map_first_some
      (fun (Endpoints.Page.W' endpoint) ->
        Madge.apply' (Endpoints.Page.route endpoint) (fun () -> set_val endpoint) (Madge.Request.make ~meth: GET ~uri ~body: "")
      )
      (Endpoints.Page.all' ())
  in
  List.filter_map (set_val % snd) (get ())

(** Returns all the books whose page is present in the history. *)
let get_books () =
  let book_val : type a r. (a, Model.Book.t Entry.id option, r) Endpoints.Page.t -> a = function
    | Book -> (fun _ id -> Some id)
    (* everything else we ignore *)
    | endpoint -> Endpoints.Page.consume None endpoint
  in
  let book_val uri : Model.Book.t Entry.id option =
    Option.join @@
    Option.map (fun f -> f ()) @@
    List.map_first_some
      (fun (Endpoints.Page.W' endpoint) ->
        Madge.apply' (Endpoints.Page.route endpoint) (fun () -> book_val endpoint) (Madge.Request.make ~meth: GET ~uri ~body: "")
      )
      (Endpoints.Page.all' ())
  in
  List.filter_map (book_val % snd) (get ())
