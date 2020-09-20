open Nes
include Dancelor_common_model.VersionFilter

let tune_author = tune_author >=>| Lwt_list.map_s Credit.get
