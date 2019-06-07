open Nes
include Dancelor_common_model.Program

let sets = sets >=>| Lwt_list.map_s Set.get

let warnings _p = assert false (* FIXME *)

(* * *)

let get = Dancelor_server_database.Program.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun {a} _ ->
    get (a Arg.slug)
  )

let get_all = Dancelor_server_database.Program.get_all

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get_all @@ fun _ _ ->
    get_all ()
  )
