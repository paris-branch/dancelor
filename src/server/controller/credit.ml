open Nes
open Lwt.Syntax
open LwtOption.Syntax
open Dancelor_server_model
open QueryHelpers

let get credit : Credit.t Controller.t = fun _ ->
  Credit.get credit

let save : Credit.t Controller.t = fun query ->
  let%lwt line = query_string query "line" in
  let%lwt persons =
    query_strings_opt query "persons"
    >>=|? (Lwt_list.map_s Person.get >=>|| Lwt.return_some)
  in
  Credit.make_and_save ~line ?persons ()
