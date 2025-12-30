open Nes

type t = {
  status: Status.t; [@default Status.bot]
  privacy: Privacy.t; [@default Privacy.default]
  created_at: Datetime.t; [@key "created-at"]
  modified_at: Datetime.t; [@key "modified-at"]
}
[@@deriving yojson, show {with_path = false}, fields]

let make ?(status = Status.bot) ?(privacy = Privacy.default) ?created_at ?modified_at () =
  let now = Datetime.now () in
  {
    status;
    privacy;
    created_at = Option.value ~default: now created_at;
    modified_at = Option.value ~default: now modified_at;
  }

let update ?status ?privacy ?created_at ?modified_at meta = {
  status = Option.value ~default: meta.status status;
  privacy = Option.value ~default: meta.privacy privacy;
  created_at = Option.value ~default: meta.created_at created_at;
  modified_at = Option.value ~default: meta.modified_at modified_at;
}
