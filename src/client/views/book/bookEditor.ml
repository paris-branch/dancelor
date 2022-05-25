open Nes
open Dancelor_client_model

type t = {
  mutable title : string;
  mutable sets : (Set.t Slug.t * Set.t) option array;
  mutable count : int;
}

let create () =
  { title = "";
    sets = Array.make 2 None;
    count = 0;
  }

let title t =
  t.title

let set_title t title =
  t.title <- title

let insert t slug i =
  if Array.length t.sets = t.count then begin
    let new_sets = Array.make (t.count * 2) None in
    Array.blit t.sets 0 new_sets 0 t.count;
    t.sets <- new_sets;
  end;
  for idx = t.count-1 downto i do
    t.sets.(idx+1) <- t.sets.(idx)
  done;
  t.count <- t.count + 1;
  let%lwt set = Set.get slug in
  t.sets.(min t.count i) <- Some (slug, set);
  Lwt.return ()

let clear t =
  t.title <- "";
  t.count <- 0

let submit t =
  let title = t.title in
  Book.make_and_save ~title ()
