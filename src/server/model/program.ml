include Dancelor_common_model.Program

let sets p =
  let%lwt sets = sets p in
  Lwt_list.map_s Dancelor_server_database.Set.get sets

let warnings _p = assert false (* FIXME *)

(* * *)

let get = Dancelor_server_database.Program.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun query ->
    get (get_arg query Arg.slug)
  )

let get_all = Dancelor_server_database.Program.get_all

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get_all @@ fun _ ->
    get_all ()
  )
