open Dancelor_common
open Dancelor_model
open Common

module Api =
  struct
    let get query =
      let slug = query_string query "slug" in
      try
        Set.Database.get slug
        |> Set.view
        |> Set.view_to_jsonm
        |> (fun json -> success ["set", json])
      with
        Not_found -> error "this set does not exist"
  end

module Html =
  struct
    let get query =
      let view = Filename.concat (Unix.getenv "DANCELOR_VIEWS") "set.html" in
      let ichan = open_in view in
      let template = Lexing.from_channel ichan |> Mustache.parse_lx in
      close_in ichan;
      Api.get query
      |> Mustache.render template
      |> respond_html
  end
