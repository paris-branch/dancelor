open Nes
open Common

let render set set_parameters rendering_parameters =
  let book =
    let title = "" in
    let contents = [Model.Book.InlineSet (set, set_parameters)] in
    Model.Book.make ~title ~contents ()
  in
  let book_parameters =
    Model.BookParameters.make ()
  in
  let%lwt rendering_parameters =
    let%lwt pdf_metadata =
      let name = Option.value (Model.SetParameters.display_name set_parameters) ~default: (Model.Set.name set) in
      let%lwt composers = List.map Model.Person.name' <$> Model.Set.conceptors set in
      let subjects =
        match KindDance.to_simple @@ Model.Set.kind set with
        | None -> ["Medley"]
        | Some (n, bars, base) ->
          [
            KindBase.to_pretty_string ~capitalised: true base;
            spf "%dx%d" n bars;
          ]
      in
      lwt @@
        RenderingParameters.update_pdf_metadata
          ~title: (String.replace_empty ~by: name)
          ~composers: (List.replace_nil ~by: composers)
          ~subjects: (List.replace_nil ~by: subjects)
    in
    lwt @@
      RenderingParameters.update ~pdf_metadata rendering_parameters
  in
  Book.Pdf.render book book_parameters rendering_parameters

let get env id _slug set_parameters rendering_parameters =
  match%lwt Model.Set.get id with
  | None -> Permission.reject_can_get ()
  | Some set ->
    Permission.assert_can_get env set;%lwt
    let%lwt path_pdf = render (Entry.value set) set_parameters rendering_parameters in
    Madge_server.respond_file ~content_type: "application/pdf" ~fname: path_pdf
