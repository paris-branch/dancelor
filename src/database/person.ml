module Model = Dancelor_model

let prefix = "person"

let db = Hashtbl.create 8

let person_from_json slug json =
  Model.Person.make_unsafe
    ~slug
    ~name:Ezjsonm.(get_string (find json ["name"]))
       
let load_person slug =
  Storage.read_json prefix slug "meta.json"
  |> person_from_json slug
  |> Hashtbl.add db slug

let () =
  Storage.list_entries prefix
  |> List.iter load_person

let get = Hashtbl.find db 
