open Nes
open Js_of_ocaml
open Dancelor_client_model

module Html = Dom_html

let js = Js.string

type cached_version = {
  slug : Version.t Slug.t;
  version : Version.t;
  tune : Tune.t
}

type t = {
  mutable name : string;
  mutable kind : string;
  mutable conceptor : (Person.t Slug.t * Person.t) option;
  mutable for_book : (Book.t Slug.t * Book.t) option;
  mutable versions : cached_version option array;
  mutable order : string;
  mutable count : int;
}

let create () =
  {
    name = "";
    kind = "";
    conceptor = None;
    for_book = None;
    versions = Array.make 2 None;
    order = "";
    count = 0;
  }

let name t =
  t.name

let set_name t name =
  t.name <- name

let kind t =
  t.kind

let set_kind t kind =
  t.kind <- kind

let order t = t.order

let set_order t order =
  t.order <- order

let conceptor t =
  let%opt (_, cr) = t.conceptor in
  Some cr

let set_conceptor t slug =
  let%lwt conceptor = Person.get slug in
  t.conceptor <- Some (slug, conceptor);
  Lwt.return ()

let remove_conceptor t =
  t.conceptor <- None

let for_book t =
  let%opt (_, bk) = t.for_book in
  Some bk

let set_for_book t slug =
  let%lwt book = Book.get slug in
  t.for_book <- Some (slug, book);
  Lwt.return ()

let remove_for_book t =
  t.for_book <- None

let count t =
  t.count

let insert t slug i =
  if Array.length t.versions = t.count then begin
    let new_versions = Array.make (t.count * 2) None in
    Array.blit t.versions 0 new_versions 0 t.count;
    t.versions <- new_versions;
  end;
  for idx = t.count-1 downto i do
    t.versions.(idx+1) <- t.versions.(idx)
  done;
  t.count <- t.count + 1;
  let%lwt version = Version.get slug in
  let%lwt tune = Version.tune version in
  t.versions.(min t.count i) <- Some {version; tune; slug};
  Lwt.return ()

let add t slug =
  insert t slug t.count

let get t i =
  if i < 0 || i >= t.count then
    None
  else
    t.versions.(i)

let remove t i =
  if i >= 0 && i < t.count then begin
    t.versions.(i) <- None;
    for j = i + 1 to t.count - 1 do
      t.versions.(j-1) <- t.versions.(j);
      t.versions.(j) <- None;
    done;
    t.count <- t.count - 1
  end

let move_up t i =
  if i > 0 && i < t.count then begin
    let tmp = t.versions.(i-1) in
    t.versions.(i-1) <- t.versions.(i);
    t.versions.(i) <- tmp
  end

let move_down t i =
  move_up t (i+1)

let iter t f =
  for i = 0 to t.count - 1 do
    match t.versions.(i) with
    | None -> ()
    | Some version -> f i version
  done

let fold t f acc =
  let acc = ref acc in
  for i = t.count - 1 downto 0 do
    match t.versions.(i) with
    | None -> ()
    | Some version -> acc := f i version !acc
  done;
  !acc

let list_versions t =
  fold t (fun _ version acc -> version::acc) []

let list_tunes t =
  fold t (fun _ version acc -> version.tune::acc) []

let clear t =
  t.name <- "";
  t.kind <- "";
  t.count <- 0;
  t.conceptor <- None;
  t.for_book <- None;
  t.order <- ""

let save t =
  Js.Optdef.case Html.window##.localStorage
    (fun () -> ())
    (fun local_storage ->
       let versions =
         list_versions t
         |> List.map (fun t -> t.slug)
         |> List.map Slug.to_string
         |> String.concat ";"
       in
       begin match t.conceptor with
         | None -> ()
         | Some (slug, _) -> local_storage##setItem (js "composer.conceptor") (js (Slug.to_string slug))
       end;
       begin match t.for_book with
         | None -> ()
         | Some (slug, _) -> local_storage##setItem (js "composer.for_book") (js (Slug.to_string slug))
       end;
       local_storage##setItem (js "composer.name") (js t.name);
       local_storage##setItem (js "composer.kind") (js t.kind);
       local_storage##setItem (js "composer.versions") (js versions);)

let load t =
  Js.Optdef.case Html.window##.localStorage
    (fun () -> Lwt.return ())
    (fun local_storage ->
       let name, kind, versions, for_book, deviser =
         local_storage##getItem (js "composer.name"),
         local_storage##getItem (js "composer.kind"),
         local_storage##getItem (js "composer.versions"),
         local_storage##getItem (js "composer.for_book"),
         local_storage##getItem (js "composer.deviser")
       in
       let open Lwt in
       Js.Opt.case name (fun () -> ())
         (fun name -> t.name <- Js.to_string name);
       Js.Opt.case kind (fun () -> ())
         (fun kind -> t.kind <- Js.to_string kind);
       Js.Opt.case versions (fun () -> Lwt.return ())
         (fun versions ->
            String.split_on_char ';' (Js.to_string versions)
            |> List.filter (fun s -> s <> " " && s <> "")
            |> List.map Slug.unsafe_of_string
            |> Lwt_list.iteri_p (fun idx slug -> insert t slug idx))
       >>= (fun () ->
           Js.Opt.case for_book (fun () -> Lwt.return ())
             (fun book -> set_for_book t (Slug.unsafe_of_string (Js.to_string book))))
       >>= (fun () ->
           Js.Opt.case deviser (fun () -> Lwt.return ())
             (fun conceptor -> set_conceptor t (Slug.unsafe_of_string (Js.to_string conceptor)))))

let add_to_storage slug =
  Js.Optdef.case Html.window##.localStorage
    (fun () -> ())
    (fun local_storage ->
       let versions = local_storage##getItem (js "composer.versions") in
       Js.Opt.case versions
         (* No versions in storage yet, we add this one *)
         (fun () -> local_storage##setItem (js "composer.versions") (js (Slug.to_string slug)))
         (* This editor already contains versions, we add the new one at the tail *)
         (fun versions ->
            let new_versions = String.cat
                (Js.to_string versions)
                (String.cat ";" (Slug.to_string slug))
            in local_storage##setItem (js "composer.versions") (js new_versions)
         )
    )

let erase_storage _ =
  Js.Optdef.case Html.window##.localStorage
    (fun () -> ())
    (fun local_storage ->
       local_storage##removeItem (js "composer.name");
       local_storage##removeItem (js "composer.kind");
       local_storage##removeItem (js "composer.deviser");
       local_storage##removeItem (js "composer.for_book");
       local_storage##removeItem (js "composer.order");
       local_storage##removeItem (js "composer.versions"))

let submit_updated_book set opt_book =
  match opt_book with
  | None -> Lwt.return ()
  | Some (slug, _) ->
    let%lwt book = Book.get slug in
    let title = Book.title book in
    let date = Book.date book in
    let%lwt contents = Book.contents book in
    let%lwt set = set in
    let contents = contents @ [Set (set, SetParameters.none)] in
    let modified_at = Datetime.now () in
    let created_at = Book.created_at book in
    Book.update ~slug ~title ?date ~contents ~modified_at ~created_at ()

let submit t =
  let versions = fold t (fun _ version acc -> version.version :: acc) [] in
  let contents = List.map (fun version -> (version, VersionParameters.none)) versions in
  let kind = Kind.Dance.of_string t.kind in
  let order = SetOrder.of_string t.order in
  let modified_at = Datetime.now () in
  let created_at = Datetime.now () in
  let answer =
    Set.make_and_save ~kind ~name:t.name ~contents
      ~order ?conceptors:(Option.map List.singleton (conceptor t)) ~modified_at ~created_at ()
  in
  Lwt.on_success answer
    (fun _ -> erase_storage t;
      Lwt.on_success (submit_updated_book answer t.for_book) (fun _ -> ()));
  answer
