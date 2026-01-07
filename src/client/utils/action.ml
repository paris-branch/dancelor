open Nes
open Html

let delete ~onclick ~model () =
  Button.make
    ~classes: ["btn-warning"]
    ~onclick: (fun _ ->
      match%lwt onclick () with
      | Ok() ->
        Toast.open_ ~title: "Deleted" [
          txt @@ "The " ^ model ^ " has now been deleted from Dancelor. Run, you fools!";
        ];
        lwt_unit
      | Error Madge_client.Http {status = `Bad_request; _} ->
        Toast.open_ ~title: "Could not delete" [
          txt @@ "Dancelor refused to delete the " ^ model ^ ", probably because there are other objects that depend on it.";
        ];
        lwt_unit
      | Error e -> raise (Madge_client.Error e)
    )
    ~icon: (Action Delete)
    ~label: "Delete"
    ~dropdown: true
    ()

let scddb type_ id =
  Button.make_a
    ~label: "See on SCDDB"
    ~icon: (Action See_outside)
    ~href: (S.const @@ Uri.to_string @@ Common.SCDDB.entry_uri type_ id)
    ~dropdown: true
    ()
