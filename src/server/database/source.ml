open Dancelor_common

module Queries_sql = Queries_sql.Sqlgg(Sqlgg_mariadb_lwt)

include Tables.Source
let delete = make_delete Tables.reverse_dependencies_of

let with_cover id f =
  Connection.with_ @@ fun db ->
  let%lwt cover = Queries_sql.Fold.get_source_cover db ~id: (Entry.Id.to_string id) (fun ~cover _ -> cover) None in
  match cover with
  | None -> f None
  | Some cover ->
    Lwt_io.with_temp_file (fun (fname, ochan) ->
      Lwt_io.write ochan cover;%lwt
      f (Some fname)
    )
