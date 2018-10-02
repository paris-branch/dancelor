include Cohttp_lwt_unix
include Dancelor_model
module Database = Dancelor_database

let respond ?(status=`OK) json =
  Server.respond_string ~status ~body:(Ezjsonm.to_string json) ()

let success = function
  | `O l -> `O (("success", `Bool true) :: l) |> respond
  | _ -> assert false

let error status msg =
  `O [
      "success", `Bool false;
      "message", `String msg
    ]
  |> respond ~status
