open Nes
include Tables.Source

let get slug = get slug
let get_all () = get_all ()

let with_cover slug f =
  Storage.with_entry_file "source" (Slug.to_string slug) "cover.webp" @@ fun fname ->
  f (if Sys.file_exists fname then Some fname else None)
