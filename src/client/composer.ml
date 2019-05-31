open Js_of_ocaml
open Dancelor_client_model
open Dancelor_common

module Html = Dom_html

let js = Js.string

type cached_tune = {
  slug : string;
  tune : Tune.t;
  group : TuneGroup.t
}

type t = {
  mutable name : string;
  mutable kind : string;
  mutable tunes : cached_tune option array;
  mutable count : int;
}

let create () =
{
  name = "";
  kind = "";
  tunes = Array.make 2 None;
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

let count t =
  t.count

let insert t slug i =
  if Array.length t.tunes = t.count then begin
    let new_tunes = Array.make (t.count * 2) None in
    Array.blit t.tunes 0 new_tunes 0 t.count;
    t.tunes <- new_tunes;
  end;
  for idx = t.count-1 downto i do
    t.tunes.(idx+1) <- t.tunes.(idx)
  done;
  t.count <- t.count + 1;
  let%lwt tune = Tune.get slug in
  let%lwt group = Tune.group tune in
  t.tunes.(min t.count i) <- Some {tune; group; slug};
  Lwt.return ()

let add t slug =
  insert t slug t.count

let get t i =
  if i < 0 || i >= t.count then
    None
  else
    t.tunes.(i)

let remove t i =
  if i >= 0 && i < t.count then begin
    t.tunes.(i) <- None;
    for j = i + 1 to t.count - 1 do
      t.tunes.(j-1) <- t.tunes.(j);
      t.tunes.(j) <- None;
    done;
    t.count <- t.count - 1
  end

let move_up t i =
  if i > 0 && i < t.count then begin
    let tmp = t.tunes.(i-1) in
    t.tunes.(i-1) <- t.tunes.(i);
    t.tunes.(i) <- tmp
  end

let move_down t i =
  move_up t (i+1)

let iter t f =
  for i = 0 to t.count - 1 do
    match t.tunes.(i) with
    | None -> ()
    | Some tune -> f i tune
  done

let fold t f acc =
  let acc = ref acc in
  for i = t.count - 1 downto 0 do
    match t.tunes.(i) with
    | None -> ()
    | Some tune -> acc := f i tune !acc
  done;
  !acc

let list_tunes t =
  fold t (fun _ tune acc -> tune::acc) []

let clear t =
  t.name <- "";
  t.kind <- "";
  t.count <- 0

let save t =
  Js.Optdef.case Html.window##.localStorage
    (fun () -> ())
    (fun local_storage ->
      let tunes =
        list_tunes t
        |> List.map (fun t -> t.slug)
        |> String.concat ";"
      in
      local_storage##setItem (js "composer.name") (js t.name);
      local_storage##setItem (js "composer.kind") (js t.kind);
      local_storage##setItem (js "composer.tunes") (js tunes))

let load t =
  Js.Optdef.case Html.window##.localStorage
    (fun () -> Lwt.return ())
    (fun local_storage ->
      let name, kind, tunes =
        local_storage##getItem (js "composer.name"),
        local_storage##getItem (js "composer.kind"),
        local_storage##getItem (js "composer.tunes")
      in
      Js.Opt.case name (fun () -> ())
        (fun name -> t.name <- Js.to_string name);
      Js.Opt.case kind (fun () -> ())
        (fun kind -> t.kind <- Js.to_string kind);
      Js.Opt.case tunes (fun () -> Lwt.return ())
        (fun tunes ->
          String.split_on_char ';' (Js.to_string tunes)
          |> List.filter (fun s -> s <> " " && s <> "")
          |> Lwt_list.iteri_p (fun idx slug -> insert t slug idx)))

let erase_storage _ =
  Js.Optdef.case Html.window##.localStorage
    (fun () -> ())
    (fun local_storage ->
      local_storage##removeItem (js "composer.name");
      local_storage##removeItem (js "composer.kind");
      local_storage##removeItem (js "composer.tunes"))

let submit t =
  let tunes = fold t (fun _ tune acc -> ("tunes", [tune.slug]) :: acc) [] in
  let query = (("name", [t.name]) :: ("kind", [t.kind]) :: tunes) in
  let answer = Dancelor_client_api.request 
    ~query ~reader:Set.of_yojson ~route:Router.SetSave ()
  in
  Lwt.on_success answer (fun _ -> erase_storage t);
  answer
