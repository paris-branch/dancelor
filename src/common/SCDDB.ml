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

let entry_uri (type_, id) =
  spf "%s%s/%d/" root (entry_type_to_string type_) id

let dance_uri id = entry_uri (Dance, id)
let formation_uri id = entry_uri (Formation, id)
let person_uri id = entry_uri (Person, id)
let publication_uri id = entry_uri (Publication, id)
let album_uri id = entry_uri (Album, id)
let recording_uri id = entry_uri (Recording, id)
let tune_uri id = entry_uri (Tune, id)
let list_uri id = entry_uri (List, id)

let entry_from_uri uri =
  let uri = Uri.of_string uri in
  match String.split_on_char '/' (Uri.path uri) with
  | [""; "dd"; type_; id; ""] ->
    (match entry_type_of_string type_ with
     | None -> kspf Result.error "Dancelor_common.SCDDB.entry_from_uri: no such entry type: %s" type_
     | Some type_ ->
       (match int_of_string_opt id with
        | None -> kspf Result.error "Dancelor_common.SCDDB.entry_from_uri: not a valid id: %s" id
        | Some id -> Ok (type_, id)))
  | _ -> kspf Result.error "Dancelor_common.SCDDB.entry_from_uri: could not recognise path"

let%test _ = entry_from_uri "https://my.strathspey.org/dd/person/11781/" = Ok (Person, 11781)
let%test _ = entry_from_uri "https://my.strathspey.org/dd/tune/14452/" = Ok (Tune, 14452)
let%test _ = Result.is_error @@ entry_from_uri "https://my.strathspey.org/choucroute/"
let%test _ = entry_from_uri "https://my.strathspey.org/dd/dance/1337/" = Ok (Dance, 1337)

let specific_entry_from_uri type_ uri =
  match entry_from_uri uri with
  | Error err -> Error err
  | Ok (type', _) when type_ <> type' ->
    kspf Result.error "Dancelor_common.SCDDB.*_from_uri: expected %s but got %s"
      (entry_type_to_string type_) (entry_type_to_string type')
  | Ok (_, id) -> Ok id

let dance_from_uri uri = specific_entry_from_uri Dance uri
let formation_from_uri uri = specific_entry_from_uri Formation uri
let person_from_uri uri = specific_entry_from_uri Person uri
let publication_from_uri uri = specific_entry_from_uri Publication uri
let album_from_uri uri = specific_entry_from_uri Album uri
let recording_from_uri uri = specific_entry_from_uri Recording uri
let tune_from_uri uri = specific_entry_from_uri Tune uri
let list_from_uri uri = specific_entry_from_uri List uri

let%test _ = person_from_uri "https://my.strathspey.org/dd/person/11781/" = Ok 11781
let%test _ = Result.is_error @@ person_from_uri "https://my.strathspey.org/dd/tune/14452/"
let%test _ = Result.is_error @@ person_from_uri "https://my.strathspey.org/choucroute/"
