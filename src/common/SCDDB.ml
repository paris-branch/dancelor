open Nes

let root = "https://my.strathspey.org/dd/"

type entry_type =
  | Dance
  | Formation
  | Person
  | Publication
  | Album
  | Recording
  | Tune
  | List

let entry_type_to_string = function
  | Dance -> "dance"
  | Formation -> "formation"
  | Person -> "person"
  | Publication -> "publication"
  | Album -> "album"
  | Recording -> "recording"
  | Tune -> "tune"
  | List -> "list"

let entry_type_of_string str =
  match String.lowercase_ascii str with
  | "dance" -> Some Dance
  | "formation" -> Some Formation
  | "person" -> Some Person
  | "publication" -> Some Publication
  | "album" -> Some Album
  | "recording" -> Some Recording
  | "tune" -> Some Tune
  | "list" -> Some List
  | _ -> None

type entry_id = int

type entry = entry_type * entry_id

let entry_type = fst
let entry_id = snd

let entry_url (type_, id) =
  spf "%s%s/%d/" root (entry_type_to_string type_) id

let dance_url id = entry_url (Dance, id)
let formation_url id = entry_url (Formation, id)
let person_url id = entry_url (Person, id)
let publication_url id = entry_url (Publication, id)
let album_url id = entry_url (Album, id)
let recording_url id = entry_url (Recording, id)
let tune_url id = entry_url (Tune, id)
let list_url id = entry_url (List, id)

let entry_from_uri uri =
  let uri = Uri.of_string uri in
  match String.split_on_char '/' (Uri.path uri) with
  | [""; "dd"; type_; id; ""] ->
    (match entry_type_of_string type_ with
     | None -> error_fmt "Dancelor_common.SCDDB.entry_from_uri: no such entry type: %s" type_
     | Some type_ ->
       (match int_of_string_opt id with
        | None -> error_fmt "Dancelor_common.SCDDB.entry_from_uri: not a valid id: %s" id
        | Some id -> Ok (type_, id)))
  | _ -> error_fmt "Dancelor_common.SCDDB.entry_from_uri: could not recognise path"

let%test _ = entry_from_uri "https://my.strathspey.org/dd/person/11781/" = Ok (Person, 11781)
let%test _ = entry_from_uri "https://my.strathspey.org/dd/tune/14452/" = Ok (Tune, 14452)
let%test _ = Result.is_error @@ entry_from_uri "https://my.strathspey.org/choucroute/"
