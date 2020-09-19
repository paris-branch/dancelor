include Tables.Version

let get slug = get slug
let get_all () = get_all ()

let read_content version =
  read_separated_file version "content.ly"

let write_content version content =
  write_separated_file version "content.ly" content
