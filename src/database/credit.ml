module Model = Dancelor_model

let prefix = "credit"

let db = Hashtbl.create 8

let credit_from_json slug json =
  Model.Credit.make_unsafe
    ~slug
    ~credit:Ezjsonm.(get_string (find json ["credit"]))
    ~persons:(Ezjsonm.(get_list get_string (find json ["persons"]))
              |> List.map Person.get)

let load_credit slug =
  Storage.read_json prefix slug "meta.json"
  |> credit_from_json slug
  |> Hashtbl.add db slug

let () =
  Sys.readdir prefix
  |> Array.iter load_credit
