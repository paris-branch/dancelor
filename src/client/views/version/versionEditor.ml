open Nes
open Dancelor_client_model

type t = {
  mutable tune: (Tune.t Slug.t * Tune.t) option;
  mutable bars: string;
  mutable key: string;
  mutable structure: string;
  mutable arranger: (Credit.t Slug.t * Credit.t) option;
  mutable remark: string;
  mutable disambiguation: string;
  mutable content: string;
}

let create () =
  {
    tune = None;
    bars = "";
    key = "";
    arranger = None;
    structure = "";
    remark = "";
    disambiguation = "";
    content = ""
  }

let tune t =
  let%opt (_, tune) = t.tune in
  Some tune

let set_tune t slug =
  let%lwt tune = Tune.get slug in
  t.tune <- Some (slug, tune);
  Lwt.return ()

let remove_tune t =
  t.tune <- None

let arranger t =
  let%opt (_, cr) = t.arranger in
  Some cr

let set_arranger t slug =
  let%lwt arranger = Credit.get slug in
  t.arranger <- Some (slug, arranger);
  Lwt.return ()

let remove_arranger t =
  t.arranger <- None

let bars t =
  t.bars

let set_bars t bars =
  t.bars <- bars

let key t =
  t.key

let set_key t key =
  t.key <- key

let structure t =
  t.structure

let set_structure t s =
  t.structure <- s

let remark t =
  t.remark

let set_remark t r =
  t.remark <- r

let disambiguation t =
  t.disambiguation

let set_disambiguation t dis =
  t.disambiguation <- dis

let content t =
  t.content

let set_content t content =
  t.content <- content

let clear t =
  t.tune <- None;
  t.bars <- "";
  t.key <- "";
  t.arranger <- None;
  t.structure <- "";
  t.remark <- "";
  t.disambiguation <- "";
  t.content <- ""

let submit t =
  let tune =
    match tune t with
    | None -> failwith "Empty tune"
    | Some tune -> tune
  in
  let bars = int_of_string t.bars in
  let key = Music.key_of_string t.key in
  let arranger = arranger t in
  let structure = t.structure in
  let remark = if t.remark = "" then None else Some t.remark in
  let disambiguation = if t.disambiguation = "" then None else Some t.disambiguation in
  let content = t.content in
  Version.make_and_save ~tune ~bars ~key ~structure ?arranger ?remark ?disambiguation ~content ()
