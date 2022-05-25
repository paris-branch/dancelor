open Nes
open Dancelor_client_model

type t = {
  mutable title : string;
  mutable sets : (Set.t Slug.t * Set.t) option array
}

let create () =
  { title = "";
    sets = Array.make 2 None
  }

let title t =
  t.title

let set_title t title =
  t.title <- title

let clear t =
  t.title <- ""

let submit t =
  let title = t.title in
  Book.make_and_save ~title ()
