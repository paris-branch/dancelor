open Nes
open Madge
open Model_new
open Model_builder.Core
module Filter = Filter_builder.Core

type (_, _, _) t =
  | For_user_row : (User_id.t -> 'w, 'w, Person_row.t option) t (* FIXME: move to user endpoints *)
  | Create : (Person.t -> 'w, 'w, Person_id.t) t
  | Search : (Slice.t -> (Person.t, Filter.Person.t) Formula_entry.public -> 'w, 'w, Person_row.t search_result) t
  | Get : (Person_id.t -> 'w, 'w, Person.entry) t (* FIXME: remove *)
  | Get_row : (Person_id.t -> 'w, 'w, Person_row.t) t
  | Get_view : (Person_id.t -> 'w, 'w, Person_view.t) t
  | Update : (Person_id.t -> Person.t -> 'w, 'w, unit) t
  | Delete : (Person_id.t -> 'w, 'w, unit) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | For_user_row -> literal "for-user" @@ variable (module User_id) @@ literal "row" @@ get (module JOption(Person_row))
    | Create -> body "person" (module Person) @@ post (module Person_id)
    | Search -> query "slice" (module Slice) @@ query "filter" (module Formula_entry.JPublic(Person)(Filter.Person)) @@ get (module Utils.Search_result(Person_row))
    | Get -> variable (module Person_id) @@ get (module Entry.JPublic(Person))
    | Get_row -> variable (module Person_id) @@ literal "row" @@ get (module Person_row)
    | Get_view -> variable (module Person_id) @@ literal "view" @@ get (module Person_view)
    | Update -> variable (module Person_id) @@ body "person" (module Person) @@ put (module JUnit)
    | Delete -> variable (module Person_id) @@ delete (module JUnit)
