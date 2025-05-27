open Nes

let root =
  Uri.make
    ~scheme: "https"
    ~host: "my.strathspey.org"
    ~path: "dd"
    ()

type entry_type =
  | Dance
  | Formation
  | Person
  | Publication
  | Album
  | Recording
  | Tune
  | List
[@@deriving yojson]

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
[@@deriving yojson]

type entry = entry_type * entry_id
[@@deriving yojson]

let entry_type = fst
let entry_id = snd

let entry_uri (type_, id) =
  Uri.with_path root @@ spf "%s/%s/%d/" (Uri.path root) (entry_type_to_string type_) id

let dance_uri id = entry_uri (Dance, id)
let formation_uri id = entry_uri (Formation, id)
let person_uri id = entry_uri (Person, id)
let publication_uri id = entry_uri (Publication, id)
let album_uri id = entry_uri (Album, id)
let recording_uri id = entry_uri (Recording, id)
let tune_uri id = entry_uri (Tune, id)
let list_uri id = entry_uri (List, id)

let entry_from_uri uri =
  match String.split_on_char '/' (Uri.path uri) with
  | [""; "dd"; type_; id; ""] ->
    (
      match entry_type_of_string type_ with
      | None -> kspf error "Dancelor_common.SCDDB.entry_from_uri: no such entry type: %s" type_
      | Some type_ ->
        (
          match int_of_string_opt id with
          | None -> kspf error "Dancelor_common.SCDDB.entry_from_uri: not a valid id: %s" id
          | Some id -> Ok (type_, id)
        )
    )
  | _ -> kspf error "Dancelor_common.SCDDB.entry_from_uri: could not recognise path"

let%test _ = entry_from_uri @@ Uri.of_string "https://my.strathspey.org/dd/person/11781/" = Ok (Person, 11781)
let%test _ = entry_from_uri @@ Uri.of_string "https://my.strathspey.org/dd/tune/14452/" = Ok (Tune, 14452)
let%test _ = Result.is_error @@ entry_from_uri @@ Uri.of_string "https://my.strathspey.org/choucroute/"
let%test _ = entry_from_uri @@ Uri.of_string "https://my.strathspey.org/dd/dance/1337/" = Ok (Dance, 1337)

let specific_entry_from_uri type_ uri =
  match entry_from_uri uri with
  | Error err -> Error err
  | Ok (type', _) when type_ <> type' ->
    kspf
      error
      "The given entry is a %s but a %s was expected"
      (entry_type_to_string type')
      (entry_type_to_string type_)
  | Ok (_, id) -> Ok id

let entry_from_string type_ string =
  match int_of_string_opt string with
  | Some entry_id -> Ok entry_id
  | None -> specific_entry_from_uri type_ (Uri.of_string string)
