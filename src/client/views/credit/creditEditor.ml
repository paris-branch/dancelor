open Nes
open Js_of_ocaml
open Dancelor_client_model

module Html = Dom_html

let js = Js.string

type cached_person = {
  person : Person.t;
  slug : Person.t Slug.t
}

type t = {
  mutable name : string;
  mutable persons : [`Edit of string | `Person of cached_person] option array;
  mutable count : int;
  mutable scddb_id : int option;
}

let create () =
{
  name = "";
  persons = Array.make 2 None;
  count = 0;
  scddb_id = None;
}

let name t =
  t.name

let set_name t name =
  t.name <- name

let count t =
  t.count

let scddb_id t =
  t.scddb_id

let set_scddb_id t id =
  t.scddb_id <- Some id

let set_field t i v =
  match t.persons.(i) with
  | Some (`Edit _) -> t.persons.(i) <- Some (`Edit v)
  | _ -> assert false

let insert t data i =
  if Array.length t.persons = t.count then begin
    let new_persons = Array.make (t.count * 2) None in
    Array.blit t.persons 0 new_persons 0 t.count;
    t.persons <- new_persons ;
  end;
  for idx = t.count-1 downto i do
    t.persons.(idx+1) <- t.persons.(idx)
  done;
  t.count <- t.count + 1;
  match data with
  | `Slug slug ->
    let%lwt person = Person.get slug in
    t.persons.(min t.count i) <- Some (`Person {person; slug});
    Lwt.return ()
  | `New str ->
    t.persons.(min t.count i) <- Some (`Edit str);
    Lwt.return ()

let add t slug =
  insert t slug t.count

let get t i =
  if i < 0 || i >= t.count then
    None
  else
    t.persons.(i)

let remove t i =
  if i >= 0 && i < t.count then begin
    t.persons.(i) <- None;
    for j = i + 1 to t.count - 1 do
      t.persons.(j-1) <- t.persons.(j);
      t.persons.(j) <- None;
    done;
    t.count <- t.count - 1
  end

let move_up t i =
  if i > 0 && i < t.count then begin
    let tmp = t.persons.(i-1) in
    t.persons.(i-1) <- t.persons.(i);
    t.persons.(i) <- tmp
  end

let move_down t i =
  move_up t (i+1)

let fold t f acc =
  let rec aux acc i =
    if i < 0 then Lwt.return acc
    else begin
      match t.persons.(i) with
      | None -> aux acc (i-1)
      | Some person ->
        Lwt.bind (f i person acc) (fun acc' -> aux acc' (i-1))
    end
  in
  aux acc (t.count - 1)

let iter t f =
  for i = 0 to t.count - 1 do
    match t.persons.(i) with
    | None -> ()
    | Some p -> f i p
  done

let list_persons t =
  fold t (fun _ person acc -> Lwt.return (person::acc)) []

let clear t =
  t.name <- "";
  t.count <- 0;
  t.scddb_id <- None

let submit t =
  let save_and_get_person = function
    | `Edit name ->
      Person.make_and_save ~name ()
    | `Person p ->
      Lwt.return p.person
  in
  let%lwt persons =
    fold t (fun _ person acc ->
      Lwt.map (fun p -> p :: acc) (save_and_get_person person))
      []
  in
  Credit.make_and_save ~line:t.name ~persons ?scddb_id:t.scddb_id ()
