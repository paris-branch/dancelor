open Nes
include Dancelor_common_model.TuneFilter

let group_author = group_author >=>| Lwt_list.map_s Credit.get
