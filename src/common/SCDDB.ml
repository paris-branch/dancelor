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
