open Dancelor_client_html
module Model = Dancelor_client_model

(* TODO: Filter out from search a deviser that has already been selected. *)

type 'model t = {
  inner_signal : 'model list S.t;
  signal : ('model list, string) Result.t S.t;
  set : 'model list -> unit;
  search :
    (
      Model.Common.Slice.t ->
      string ->
      (int * 'model list, string) result Lwt.t
    );
  make_result :
    (
      ?onclick: (unit -> unit) ->
      ?prefix: Html_types.td elt list ->
      ?suffix: Html_types.td elt list ->
      'model ->
      Html_types.tr elt
    );
}

let make ~search ~make_result f =
  let (inner_signal, set) = S.create [] in
  let signal = S.map f inner_signal in
  {inner_signal; signal; set; search; make_result}

let signal s = s.signal

let clear s =
  (* FIXME: Also set the search bar to empty text. *)
  s.set []

let render s =
  div [
    tablex [
      R.tbody (
        Fun.flip S.map s.inner_signal @@ List.map (fun person ->
            s.make_result
              ~suffix:[
                td [i ~a:[a_class ["material-symbols-outlined"]] [txt "delete"]]
              ]
              person
          )
      )
    ];
    QuickSearchBar.make_and_render
      ~number_of_results: 5
      ~placeholder: "Add a deviser (magic search)"
      ~search: s.search
      ~make_result: (fun person ->
          Lwt.return @@ s.make_result
            ~onclick:(fun () ->
                s.set (S.value s.inner_signal @ [person])
                (* FIXME: Also set the search bar to empty text. *)
              )
            person
        )
      ();
    div ~a:[a_class ["message-box"]] [];
  ]
