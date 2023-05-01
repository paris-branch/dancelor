open Nes
open Dancelor_common
open Dancelor_client_model

type t = {
  mutable name : string;
  mutable alternative : string;
  mutable kind : string;
  mutable author : (Credit.t Slug.t * Credit.t) option;
  mutable dances : (Dance.t Slug.t * Dance.t) option array;
  mutable remark : string;
  mutable scddb_id : string;
  mutable count : int;
}

let create () =
  {
    name = "";
    alternative = "";
    kind = "";
    author = None;
    count = 0;
    dances = Array.make 1 None;
    remark = "";
    scddb_id = "";
  }

let name t =
  t.name

let set_name t name =
  t.name <- name

let alternative t =
  t.alternative

let set_alternative t name =
  t.alternative <- name

let kind t =
  t.kind

let set_kind t kind =
  t.kind <- kind

let author t =
  let%opt (_, cr) = t.author in
  Some cr

let set_author t slug =
  let%lwt author = Credit.get slug in
  t.author <- Some (slug, author);
  Lwt.return ()

let remove_author t =
  t.author <- None

let count t =
  t.count

let insert t slug i =
  if Array.length t.dances = t.count then begin
    let new_dances = Array.make (t.count * 2) None in
    Array.blit t.dances 0 new_dances 0 t.count;
    t.dances <- new_dances;
  end;
  for idx = t.count-1 downto i do
    t.dances.(idx+1) <- t.dances.(idx)
  done;
  t.count <- t.count + 1;
  let%lwt dance = Dance.get slug in
  t.dances.(min t.count i) <- Some (slug, dance);
  Lwt.return ()

let add t slug =
  insert t slug t.count

let get t i =
  if i < 0 || i >= t.count then
    None
  else
    t.dances.(i)

let remove t i =
  if i >= 0 && i < t.count then begin
    t.dances.(i) <- None;
    for j = i + 1 to t.count - 1 do
      t.dances.(j-1) <- t.dances.(j);
      t.dances.(j) <- None;
    done;
    t.count <- t.count - 1
  end

let iter t f =
  for i = 0 to t.count - 1 do
    match t.dances.(i) with
    | None -> ()
    | Some dance -> f i dance
  done

let fold t f acc =
  let acc = ref acc in
  for i = t.count - 1 downto 0 do
    match t.dances.(i) with
    | None -> ()
    | Some dance -> acc := f i dance !acc
  done;
  !acc

let list_dances t =
  fold t (fun _ dance acc -> (snd dance) :: acc) []

let remark t =
  t.remark

let set_remark t remark =
  t.remark <- remark

let scddb_id t =
  t.scddb_id

let set_scddb_id t id =
  t.scddb_id <- id

let clear t =
  t.name <- "";
  t.alternative <- "";
  t.kind <- "";
  t.author <- None;
  t.count <- 0;
  t.dances <- Array.make 1 None;
  t.scddb_id <- ""

let submit t =
  let name = t.name in
  let alternative_names = if t.alternative = "" then [] else [t.alternative] in
  let kind = Kind.base_of_string t.kind in
  let dances = list_dances t in
  let remark = if t.remark = "" then None else Some t.remark in
  let scddb_id =
    if t.scddb_id = "" then
      None
    else
      try%opt int_of_string_opt t.scddb_id
      with _ -> Result.to_option (SCDDB.tune_from_uri t.scddb_id)
  in
  let modified_at = Datetime.now () in
  let created_at = Datetime.now () in
  Tune.make_and_save ~name ~alternative_names ~kind ?author:(author t)
    ~dances ?remark ?scddb_id ~modified_at ~created_at ()
