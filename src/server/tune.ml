open Dancelor_model
open Common

module Api =
  struct
    let get query =
      let slug = query_string query "slug" in
      try
        Tune.Database.get slug
        |> Tune.view
        |> Tune.view_to_jsonm
        |> (fun json -> success ["tune", json])
      with
        Not_found -> error "this tune does not exist"
  end

module Html =
  struct
    let get query =
      let view = Filename.concat (Unix.getenv "DANCELOR_VIEWS") "tune.html" in
      let ichan = open_in view in
      let template = Lexing.from_channel ichan |> Mustache.parse_lx in
      close_in ichan;
      Api.get query
      |> Mustache.render template
      |> respond_html
  end
