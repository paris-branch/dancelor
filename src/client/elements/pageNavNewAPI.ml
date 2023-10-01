open Nes
open Dancelor_client_html.NewAPI

type state = {
  current_page : int; (* first page is [0] *)
  entries_per_page : int;
  number_of_entries : int;
}

let number_of_pages state =
  (state.number_of_entries
   + state.entries_per_page
   - 1)
  / state.entries_per_page

let current_pagination state =
  Dancelor_common_model.Pagination.{
    start = state.current_page * state.entries_per_page;
    end_ = (state.current_page + 1) * state.entries_per_page;
  }

type t = {
  signal : state React.signal;
  setter : state -> unit;
  updater : (state -> state) -> unit;
}

let create ~number_of_entries ~entries_per_page =
  let initial = {
    current_page = 0;
    entries_per_page;
    number_of_entries = 0;
  }
  in
  let (signal, setter) = S.create initial in
  let updater f = setter (f (S.value signal)) in
  Lwt.on_success number_of_entries (fun number_of_entries ->
      updater (fun state -> { state with number_of_entries }));
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

let status_text _pagination =
  S.const @@ "In progress"

let button_list pagination =
  S.bind pagination.signal @@ fun state ->
  S.const (
    List.init
      (number_of_pages state)
      (fun page -> button (1 + page) pagination)
  )

let render pagination =
  div ~a:[a_id "page_nav"] [
    div [R.txt (status_text pagination)];
    R.ul (button_list pagination);
  ]
