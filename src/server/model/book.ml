open Nes
module Common = Dancelor_common
module Database = Dancelor_server_database

include BookLifted

let create = Database.Book.create
let update = Database.Book.update
let save = Database.Book.save

include Common.Model.Search.Make(struct
    type value = t Database.Entry.t
    type filter = Filter.t

    let cache = Cache.create ~lifetime: 600 ()
    let get_all = Database.Book.get_all
    let filter_accepts = Filter.accepts

    let tiebreakers =
      Lwt_list.[
        decreasing (Lwt.return % date) (Option.compare PartialDate.compare);
        increasing (Lwt.return % title) String.Sensible.compare;
        increasing (Lwt.return % title) String.compare_lengths;
        increasing (Lwt.return % subtitle) String.Sensible.compare;
        increasing (Lwt.return % subtitle) String.compare_lengths;
      ]
  end)
