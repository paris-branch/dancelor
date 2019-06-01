open Dancelor_server_model
open QueryHelpers

let get credit : Credit.t Controller.t = fun _ ->
  Credit.get credit

let save : Credit.t Controller.t = fun query ->
  let%lwt line = query_string query "line" in
  let%lwt persons =
    match%lwt query_strings_opt query "persons" with
    | None -> Lwt.return_none
    | Some persons ->
      let%lwt persons = Lwt_list.map_s Person.get persons in
      Lwt.return_some persons
  in
  Credit.make_and_save ~line ?persons ()
