open Nes
open Html

type state = {
  current_page: int; (* first page is [1] *)
  entries_per_page: int;
  number_of_entries: int option; (* not sure that we know *)
}

let number_of_pages {entries_per_page; number_of_entries; _} =
  let number_of_entries = Option.value ~default: entries_per_page number_of_entries in
  (number_of_entries + entries_per_page - 1) / entries_per_page

let current_slice {current_page; entries_per_page; number_of_entries} =
  let number_of_entries = Option.value ~default: entries_per_page number_of_entries in
  Slice.make
    (* NOTE: Our page numbers start at 1. *)
    ~start: ((current_page - 1) * entries_per_page)
    ~end_excl: (min (current_page * entries_per_page) number_of_entries)
    ()

type t = {
  state: state React.signal;
  update_current_page: (int -> int) -> unit;
}

let create ~number_of_entries ~entries_per_page =
  let (current_page, set_current_page) = S.create 1 in
  let update_current_page f =
    let current_page = S.value current_page in
    set_current_page (f current_page)
  in
  let state =
    S.bind current_page @@ fun current_page ->
    S.bind number_of_entries @@ fun number_of_entries ->
    S.const
      {
        current_page;
        entries_per_page;
        number_of_entries;
      }
  in
    {state; update_current_page}

let status_text pagination =
  Fun.flip S.map pagination.state @@ fun state ->
  match state.number_of_entries with
  | None -> "Loading.."
  | Some 0 -> "No entries"
  | Some number_of_entries ->
    let slice = current_slice state in
    spf
      "Showing %d to %d of %d entries"
      (Slice.start slice + 1)
      (Slice.end_excl slice)
      number_of_entries

module Button = struct
  (** [make ~active ~enabled ~target ~text pagination] is a button that shows
      [text]. It is active (that is, it shows as different from the other) when
      the predicate [active] returns [true] on the [pagination] state. It is
      enabled (that is not grayed out and clickable) when the predicate [enabled]
      returns [true] on the [pagination] state. It changes the page to the result
      of [target] applied on the [pagination] state. *)
  let make ~active ~enabled ~target ~text pagination =
    li
      ~a: [
        R.a_class
          (
            Fun.flip S.map pagination.state @@ fun state ->
            "page-item" :: if active state then
                ["active"]
            else if enabled state then
                []
            else
                ["disabled"]
          )
      ]
      [
        a
          ~a: [
            a_class ["page-link"];
            a_onclick
              (fun _ ->
                let state = S.value pagination.state in
                if enabled state then
                  pagination.update_current_page (fun current_page -> target current_page);
                false
              )
          ]
          [txt text]
      ]

  (** A value that can be passed to [make]'s [~target] argument when the button
      is never be enabled. *)
  let no_target = fun _ ->
    failwith "Dancelor_client.Eleents.Pagination.no_target"

  (** A button that is never enabled and shows three dots. *)
  let ellipsis =
    make
      ~active: (Fun.const false)
      ~enabled: (Fun.const false)
      ~target: no_target
      ~text: "..."

  (** A button that is constantly linked to a page number. *)
  let numbered page =
    make
      ~active: (fun state -> state.current_page = page)
      ~enabled: (Fun.const true)
      ~target: (Fun.const page)
      ~text: (string_of_int page)

  (** A button that brings to the previous page. *)
  let previous =
    make
      ~active: (Fun.const false)
      ~enabled: (fun state -> state.current_page <> 1)
      ~target: (fun current_page -> current_page - 1)
      ~text: "Previous"

  (** A button that brings to the next page. *)
  let next =
    make
      ~active: (Fun.const false)
      ~enabled: (fun state -> state.current_page <> number_of_pages state)
      ~target: (fun current_page -> current_page + 1)
      ~text: "Next"
end

let button_list pagination =
  Fun.flip S.map pagination.state @@ fun state ->
  let number_of_pages = number_of_pages state
  and current_page = state.current_page
  in
  (* Select the page numbers to show. *)
  let relevant_page_numbers =
    number_of_pages
    |> Fun.flip List.init ((+) 1)
    |> List.filter
        (fun i ->
          i = 1
          || i = number_of_pages
          || abs (current_page - i) <= 2
        )
  in
  let rec numbered_buttons previous = function
    | [] -> []
    | page_number :: page_numbers ->
      (if page_number <> previous + 1 then [Button.ellipsis] else []) @
      [Button.numbered page_number] @
      numbered_buttons page_number page_numbers
  in
  List.map
    (fun button -> button pagination)
    (
      [Button.previous] @
      numbered_buttons 0 relevant_page_numbers @
        [Button.next]
    )

let render ~is_below pagination =
  nav
    ~a: [a_class ["d-md-flex"; "justify-content-between"; (if is_below then "align-items-start" else "align-items-end")]]
    [
      div [R.txt (status_text pagination)];
      R.ul
        ~a: [a_class ["pagination"; "mb-0"]]
        (button_list pagination);
    ]

let slice page_nav = S.map current_slice page_nav.state
