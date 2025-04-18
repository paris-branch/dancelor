open Nes
open Common

include ModelBuilder.Book.Build(Dance)(Set)(Tune)(Version)

let get = Database.Book.get

let create = Database.Book.create
let update = Database.Book.update
let save = Database.Book.save

include ModelBuilder.Search.Build(struct
  type value = t Entry.t
  type filter = Filter.t

  let cache = Cache.create ~lifetime: 600 ()
  let get_all = Database.Book.get_all
  let filter_accepts = Filter.accepts

  let tiebreakers =
    Lwt_list.[decreasing (Lwt.return % date) (Option.compare PartialDate.compare);
    increasing (Lwt.return % title) String.Sensible.compare;
    increasing (Lwt.return % title) String.compare_lengths;
    increasing (Lwt.return % subtitle) String.Sensible.compare;
    increasing (Lwt.return % subtitle) String.compare_lengths;
    ]
end)

module Parameters = ModelBuilder.BookParameters
