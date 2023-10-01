open Nes
open Dancelor_client_html.NewAPI

type state = {
  current_page : int;
  entries_per_page : int;
  number_of_entries : int;
  number_of_pages : int;
}

type t = {
  signal : state React.signal;
  setter : state -> unit;
  updater : (state -> state) -> unit;
}

let create ~entries_per_page =
  let initial = {
    current_page = 0;
    entries_per_page;
    number_of_entries = 0;
    number_of_pages = 0;
  }
  in
  let (signal, setter) = S.create initial in
  let updater f = setter (f (S.value signal)) in
  { signal; setter; updater }

let button page _pagination =
  li
    ~a:[a_class ["active"]]
    [
      button ~a: [
        a_button_type `Button;
      ] [
        txt (string_of_int page)
      ]
    ]

let button_list pagination =
  S.bind pagination.signal @@ fun state ->
  S.const (
    List.init state.number_of_pages (fun page ->
        button (1 + page) pagination
      )
  )

let render pagination =
  div ~a:[a_id "page_nav"] [
    div [txt "In progress"]; (* info *)
    R.ul (button_list pagination);
  ]
