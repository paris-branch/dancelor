open Nes
open Dancelor_common_model
open Dancelor_client_html

type state = {
  current_page : int; (* first page is [1] *)
  entries_per_page : int;
  number_of_entries : int;
}

let number_of_pages state =
  (state.number_of_entries
   + state.entries_per_page
   - 1)
  / state.entries_per_page

let current_pagination state =
  (* NOTE: Our page numbers start at 1. *)
  Pagination.{
    start = (state.current_page - 1) * state.entries_per_page;
    end_ = min (state.current_page * state.entries_per_page) state.number_of_entries;
  }

type t = {
  signal : state React.signal;
  setter : state -> unit;
  updater : (state -> state) -> unit;
}

(** Create a page navigation from a number of entries (as an {!Lwt} promise)
    and a number of entries per page. *)
let create ~(number_of_entries: int Lwt.t) ~(entries_per_page: int) : t =
  let initial = {
    current_page = 1;
    entries_per_page;
    number_of_entries = 0;
  }
  in
  let (signal, setter) = S.create initial in
  let updater f = setter (f (S.value signal)) in
  Lwt.on_success number_of_entries (fun number_of_entries ->
      updater (fun state -> { state with number_of_entries }));
  { signal; setter; updater }

let status_text pagination =
  Fun.flip S.map pagination.signal @@ fun state ->
  if state.number_of_entries = 0 then
    "No entries"
  else
    let pagination = current_pagination state in
    spf "Showing %i to %i of %i entries"
      (Pagination.start pagination + 1)
      (Pagination.end_ pagination)
      state.number_of_entries

module Button = struct
  (** [make ~active ~enabled ~target ~text pagination] is a button that shows
      [text]. It is active (that is, it shows as different from the other) when
      the predicate [active] returns [true] on the [pagination] state. It is
      enabled (that is not grayed out and clickable) when the predicate [enabled]
      returns [true] on the [pagination] state. It changes the page to the result
      of [target] applied on the [pagination] state. *)
  let make ~active ~enabled ~target ~text pagination =
    li
      ~a:[
        R.a_class (
          Fun.flip S.map pagination.signal @@ fun state ->
          if active state then ["active"] else []
        )
      ]
      [
        button ~a: [
          a_button_type `Button;
          R.a_class (
            Fun.flip S.map pagination.signal @@ fun state ->
            if enabled state then ["clickable"] else ["disabled"]
          );
          a_onclick
            (fun _ ->
               let state = S.value pagination.signal in
               if enabled state then
                 pagination.updater (fun state ->
                     { state with current_page = target state }
                   );
               false
            )
        ]
          [txt text]
      ]

  (** A value that can be passed to [make]'s [~target] argument when the button
      is never be enabled. *)
  let no_target =
    fun _ -> failwith "Dancelor_client.Eleents.PageNav.no_target"

  (** A button that is never enabled and shows three dots. *)
  let ellipsis =
    make
      ~active:(Fun.const false)
      ~enabled:(Fun.const false)
      ~target:no_target
      ~text:"..."

  (** A button that is constantly linked to a page number. *)
  let numbered page =
    make
      ~active:(fun state -> state.current_page = page)
      ~enabled:(Fun.const true)
      ~target:(Fun.const page)
      ~text:(string_of_int page)

  (** A button that brings to the previous page. *)
  let previous =
    make
      ~active:(Fun.const false)
      ~enabled:(fun state -> state.current_page <> 1)
      ~target:(fun state -> state.current_page - 1)
      ~text:"Previous"

  (** A button that brings to the next page. *)
  let next =
    make
      ~active:(Fun.const false)
      ~enabled:(fun state -> state.current_page <> number_of_pages state)
      ~target:(fun state -> state.current_page + 1)
      ~text:"Next"
end

let button_list pagination =
  Fun.flip S.map pagination.signal @@ fun state ->
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
      (if page_number <> previous + 1 then [Button.ellipsis] else [])
      @ [Button.numbered page_number]
      @ numbered_buttons page_number page_numbers
  in
  List.map
    (fun button -> button pagination)
    (
      [Button.previous]
      @ numbered_buttons 0 relevant_page_numbers @
      [Button.next]
    )

let render pagination =
  div ~a:[a_id "page_nav"] [
    div [R.txt (status_text pagination)];
    R.ul (button_list pagination);
  ]
