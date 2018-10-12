open Dancelor_model
open Common

module Api =
  struct
    let get query =
      let slug = query_string query "slug" in
      try
        Person.Database.get slug
        |> Person.view
        |> Person.view_to_jsonm
        |> (fun json -> success ["person", json])
      with
        Not_found -> error "this person does not exist"
  end

module Html =
  struct
    let get query =
      let view = Filename.concat (Unix.getenv "DANCELOR_VIEWS") "person.html" in
      let ichan = open_in view in
      let template = Lexing.from_channel ichan |> Mustache.parse_lx in
      close_in ichan;
      Api.get query
      |> Mustache.render template
      |> respond_html
  end
