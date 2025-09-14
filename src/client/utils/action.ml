open Nes
open Html

let delete ~onclick ~model () =
  Button.make
    ~classes: ["dropdown-item"; "btn-warning"]
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
    ~icon: "trash"
    ~label: "Delete"
    ()
