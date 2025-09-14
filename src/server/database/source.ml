open Common

include Tables.Source
let delete = make_delete Tables.reverse_dependencies_of

let with_cover id f =
  Storage.with_entry_file "source" (Entry.Id.to_string id) "cover.webp" @@ fun fname ->
  f (if Sys.file_exists fname then Some fname else None)
