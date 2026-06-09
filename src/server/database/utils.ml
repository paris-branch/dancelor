open Nes
open Dancelor_common
open Model_new

let fold_to_hashtbl fold db k =
  let tbl = Hashtbl.create 8 in
  fold db (k (fun key value () -> Hashtbl.add tbl key value)) ();%lwt
  lwt tbl

let sql_to_person_name ~id ~name ~(k : Person_name.t -> 'w) : 'w =
  k {id = Entry.Id.of_string_exn id; name}
