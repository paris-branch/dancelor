open Nes

let _key = "version"

type t = {
  slug: t Slug.t;
  status: Status.t [@default Status.bot];
  tune: TuneCore.t Slug.t;
  bars: int;
  key: Music.key;
  structure: string;
  sources: string list [@default []]; (* FIXME: remove from DB *)
  arranger: CreditCore.t Slug.t option [@default None];
  remark: string [@default ""];
  disambiguation: string [@default ""];
  broken: bool [@default false];
}
[@@deriving make, yojson]

let make
    ~slug
    ?status
    ~tune
    ~bars
    ~key
    ~structure
    ?arranger
    ?remark
    ?disambiguation
    ?broken
    ()
  =
  let%lwt tune = TuneCore.slug tune in
  let%lwt arranger =
    let%olwt arranger = Lwt.return arranger in
    let%lwt arranger = CreditCore.slug arranger in
    Lwt.return_some arranger
  in
  Lwt.return
    (
      make
        ~slug
        ?status
        ~tune
        ~bars
        ~key
        ~structure
        ~arranger
        ?remark
        ?disambiguation
        ?broken
        ()
    )

let slug t = Lwt.return t.slug
let status t = Lwt.return t.status
let tune t = Lwt.return t.tune
let bars t = Lwt.return t.bars
let key t = Lwt.return t.key
let structure t = Lwt.return t.structure
let arranger t = Lwt.return t.arranger
let remark t = Lwt.return t.remark
let disambiguation t = Lwt.return t.disambiguation
let broken t = Lwt.return t.broken

let set_broken t broken = Lwt.return { t with broken }

let equal version1 version2 =
  let%lwt slug1 = slug version1 in
  let%lwt slug2 = slug version2 in
  Lwt.return (Slug.equal slug1 slug2)
