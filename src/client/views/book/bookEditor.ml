open Nes
open Dancelor_client_model

type t = {
  mutable title : string;
  mutable date : string;
  mutable sets : (Set.t Slug.t * Set.t) option array;
  mutable count : int;
}

let create () =
  { title = "";
    date = "";
    sets = Array.make 2 None;
    count = 0;
  }

let title t =
  t.title

let set_title t title =
  t.title <- title

let date t =
  t.date

let set_date t date =
  t.date <- date

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

let add t slug =
  insert t slug t.count

let remove t i =
  if i >= 0 && i < t.count then begin
    t.sets.(i) <- None;
    for j = i + 1 to t.count - 1 do
      t.sets.(j-1) <- t.sets.(j)
    done;
    t.sets.(t.count - 1) <- None;
    t.count <- t.count - 1
  end

let move_up t i =
  if i > 0 && i < t.count then begin
    let tmp = t.sets.(i-1) in
    t.sets.(i-1) <- t.sets.(i);
    t.sets.(i) <- tmp
  end

let move_down t i =
  move_up t (i+1)

let iter t f =
  for i = 0 to t.count - 1 do
    match t.sets.(i) with
    | None -> ()
    | Some set -> f i set
  done

let fold t f acc =
  let acc = ref acc in
  for i = t.count - 1 downto 0 do
    match t.sets.(i) with
    | None -> ()
    | Some set -> acc := f i set !acc
  done;
  !acc

let clear t =
  t.title <- "";
  t.date <- "";
  t.count <- 0

let submit t =
  let title = t.title in
  let date = if t.date <> "" then Some (Date.Partial.from_string t.date) else None in
  let contents = fold t (fun _ set acc -> snd set :: acc) [] in
  let contents_and_parameters = List.map (fun set -> Book.Set (set, SetParameters.none)) contents in
  Book.make_and_save ~title ?date ~contents_and_parameters ()
