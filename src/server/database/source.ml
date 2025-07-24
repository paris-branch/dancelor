open Common
include Tables.Source

let get id = get id
let get_all () = get_all ()

let with_cover id f =
  Storage.with_entry_file "source" (Entry.Id.to_string id) "cover.webp" @@ fun fname ->
  f (if Sys.file_exists fname then Some fname else None)
