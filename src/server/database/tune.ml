include Tables.Tune

let get slug = get slug
let get_all () = get_all ()

let read_content tune =
  read_separated_file tune "content.ly"

let write_content tune content =
  write_separated_file tune "content.ly" content
