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
  mutable deviser : (Credit.t Slug.t * Credit.t) option;
  mutable versions : cached_version option array;
  mutable count : int;
}

let create () =
{
  name = "";
  kind = "";
  deviser = None;
  versions = Array.make 2 None;
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

let deviser t =
  match t.deviser with
  | None -> None
  | Some (_, cr) -> Some cr

let set_deviser t slug =
  let%lwt deviser = Credit.get slug in
  t.deviser <- Some (slug, deviser);
  Lwt.return ()

let remove_deviser t =
  t.deviser <- None

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

let clear t =
  t.name <- "";
  t.kind <- "";
  t.count <- 0;
  t.deviser <- None

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
      begin match t.deviser with
      | None -> ()
      | Some (slug, _) -> local_storage##setItem (js "composer.deviser") (js (Slug.to_string slug))
      end;
      local_storage##setItem (js "composer.name") (js t.name);
      local_storage##setItem (js "composer.kind") (js t.kind);
      local_storage##setItem (js "composer.versions") (js versions);)

let load t =
  Js.Optdef.case Html.window##.localStorage
    (fun () -> Lwt.return ())
    (fun local_storage ->
      let name, kind, versions, deviser =
        local_storage##getItem (js "composer.name"),
        local_storage##getItem (js "composer.kind"),
        local_storage##getItem (js "composer.versions"),
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
      Js.Opt.case deviser (fun () -> Lwt.return ())
        (fun deviser -> set_deviser t (Slug.unsafe_of_string (Js.to_string deviser)))))

let erase_storage _ =
  Js.Optdef.case Html.window##.localStorage
    (fun () -> ())
    (fun local_storage ->
      local_storage##removeItem (js "composer.name");
      local_storage##removeItem (js "composer.kind");
      local_storage##removeItem (js "composer.deviser");
      local_storage##removeItem (js "composer.versions"))

let submit t =
  let versions = fold t (fun _ version acc -> version.version :: acc) [] in
  let versions_and_parameters = List.map (fun version -> (version, VersionParameters.none)) versions in
  let kind = Kind.dance_of_string t.kind in
  let answer = Set.make_and_save ~kind ~name:t.name ~versions_and_parameters ?deviser:(deviser t) () in
  Lwt.on_success answer (fun _ -> erase_storage t);
  answer
