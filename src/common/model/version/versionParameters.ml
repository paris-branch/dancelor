open Nes

module Self = struct
  type t =
    { transposition  : Transposition.t option [@default None] ;
      first_bar      : int             option [@default None] [@key "first-bar"] ;

      for_dance      : DanceCore.t Slug.t option [@default None] [@key "for-dance"] ;

      clef           : Music.clef      option [@default None] ;
      display_name   : string          option [@default None] [@key "display-name"] ;
      display_author : string          option [@default None] [@key "display-author"] }
  [@@deriving make, yojson]

  let _key = "version-parameters"
end
include Self

(* FIXME: [@@deriving yojson] and [@@deriving make] do not have the same
   interpretation of [@default]. Basically, we want [@@deriving yojson] to
   interpret it, but not [@@deriving make]. This is normally possible by using
   [@yojson.default]. Current version of [@@deriving yojson] (3.5.3) does not,
   however, seem to recognise this option anymore. In the meantime, we use
   [@default] and we add a dirty fix for [@@deriving make]: *)
let make ?transposition ?clef ?first_bar ?display_name ?display_author () =
  make ~transposition ~clef ~first_bar ~display_name ~display_author ()

let transposition  p = Option.unwrap p.transposition
let first_bar      p = Option.unwrap p.first_bar

let for_dance      p = p.for_dance

let clef           p = p.clef
let display_name   p = p.display_name
let display_author p = p.display_author

let none = `Assoc [] |> of_yojson |> Result.get_ok

let default = {
  transposition  = Some Transposition.identity ;
  first_bar      = Some 1 ;
  for_dance      = None ;

  clef           = None ;
  display_name   = None ;
  display_author = None ;
}

let compose first second =
  { transposition  = Option.choose ~tie:Transposition.compose first.transposition second.transposition ;
    clef           = Option.(choose ~tie:second) first.clef           second.clef ;
    first_bar      = Option.(choose ~tie:second) first.first_bar      second.first_bar ;
    for_dance      = Option.(choose ~tie:fail)   first.for_dance      second.for_dance ;
    display_name   = Option.(choose ~tie:second) first.display_name   second.display_name ;
    display_author = Option.(choose ~tie:second) first.display_author second.display_author }

let fill = compose default
