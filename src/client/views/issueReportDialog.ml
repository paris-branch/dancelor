open Nes
open Dancelor_common
module Model = Dancelor_client_model
open Dancelor_client_html
open Dancelor_client_components

let describe page =
  match page with
  | None -> Lwt.return None
  | Some page ->
    match page with
    | PageRouter.Index
    | Explore _
    | VersionAdd
    | TuneAdd
    | SetCompose
    | BookCompose
    | BookEdit _
    | PersonAdd
    | DanceAdd ->
      Lwt.return None
    | Version {slug; _} ->
      let%lwt name = Lwt.bind (Model.Version.get slug) Model.Version.name in
      Lwt.return @@ Some ("version", name)
    | Tune {slug; _} ->
      let%lwt name = Lwt.map Model.Tune.name (Model.Tune.get slug) in
      Lwt.return @@ Some ("tune", name)
    | Set {slug; _} ->
      let%lwt name = Lwt.map Model.Set.name (Model.Set.get slug) in
      Lwt.return @@ Some ("set", name)
    | Book {slug; _} ->
      let%lwt title = Lwt.map Model.Book.title (Model.Book.get slug) in
      Lwt.return @@ Some ("book", title)
    | Dance {slug; _} ->
      let%lwt name = Lwt.map Model.Dance.name (Model.Dance.get slug) in
      Lwt.return @@ Some ("dance", name)
    | Person {slug; _} ->
      let%lwt name = Lwt.map Model.Person.name (Model.Person.get slug) in
      Lwt.return @@ Some ("person", name)

let open_ page =
  let reporter_input = Input.Text.make "" (fun s -> Ok s) in
  let%lwt source =
    Fun.flip Lwt.map (describe page) @@ function
      | None ->
        Choices.make_radios
          ~name: "Source of the issue"
          [
            Choices.choice' ~value: true [txt "Dancelor itself"] ~checked: true;
          ]
      | Some (kind, name) ->
        Choices.make_radios
          ~name: "Source of the issue"
          [
            Choices.choice'
              ~value: false
              [
                txt @@
                  spf "This %s: %s" kind name
              ];
            Choices.choice' ~value: true [txt "Dancelor itself"];
          ]
  in
  let title_input = Input.Text.make "" (fun s -> Ok s) in
  let description_input = Input.Text.make "" (fun s -> Ok s) in
  Dialog.open_ @@ fun return ->
  [
    h2 [txt "Report an issue"];
    form
      [
        Input.Text.render
          reporter_input
          ~placeholder: "Dr Jean Milligan"
          ~label: "Reporter";
        Choices.render source;
        Input.Text.render
          title_input
          ~placeholder: "Blimey, 'tis not working!"
          ~label: "Title";
        Input.Text.render_as_textarea
          description_input
          ~placeholder: "I am gutted; this knock off tune is wonky at best!"
          ~label: "Description";
        Button.group
          [
            Button.save
              ~label: ("Report", "Reporting...")
              ~disabled: (S.const false)
              ~onclick: (fun () ->
                Lwt.pmsleep 2.;%lwt
                return (Ok ());
                Lwt.return_unit
              )
              ();
            Button.cancel ~return ()
          ]
      ]
  ]
