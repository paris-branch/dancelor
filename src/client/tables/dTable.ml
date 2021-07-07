open Dancelor_client_html

let clickable_row ?href ?href_lwt cells =
  tr ~classes:["clickable"] (
    List.map
      (fun cell_lwt ->
         td [ a_lwt ~classes:["fill"] ?href ?href_lwt cell_lwt ])
      cells
  )

let map_table ~header list fun_ =
  table ~classes:["separated-table"; "visible"] [
    thead (List.map text header);
    tbody (List.map fun_ list)
  ]
