open Nes
open Common
open Html
open Utils

let dialog
    ~source_type
    ~target_type
    ~target_icon
    ~source_format
    ~target_format
    ~target_href
    ~target_filter_from_string
    ~target_filter_owners'
    ~(target_result : ?onclick: 'a -> ?context: 'b -> 'c -> 'd)
    ~target_search
    ~target_update
    ~target_get
    ~target_history
    ~target_add_source_to_content
    user
    source
  =
  let make_result ?context ~return target =
    target_result
      ?context
      target
      ~onclick: (fun () ->
        let%lwt target_value = target_add_source_to_content (Entry.value target) in
        ignore <$> target_update (Entry.id target) target_value (Entry.access target);%lwt
        Toast.open_
          ~title: (spf "Added to %s" target_type)
          [txtf "The %s " source_type;
          source_format source;
          txtf " has been added to %s " target_type;
          target_format target;
          txt " successfully.";
          ]
          ~buttons: [
            Button.make_a
              ~label: ("Go to " ^ target_type)
              ~icon: target_icon
              ~classes: ["btn-primary"]
              ~href: (S.const @@ target_href @@ Entry.id target)
              ();
          ];
        return (Some ());
        lwt_unit
      )
  in
  let quick_search =
    Components.Search.Quick.make
      ~search: (fun slice input ->
        let%rlwt filter = lwt (target_filter_from_string input) in
        (* FIXME: Rather than the entries owned by the user, we should filter
           on the entries that the user is allowed to edit, that is we should
           have filters for permissions. *)
        let filter = Formula.and_ filter (target_filter_owners' Filter.(Formula_list.exists' (User.is' user))) in
        ok <$> target_search slice filter
      )
      ()
  in
  let%lwt results_when_no_search =
    let%lwt targets = Lwt_list.map_p (Option.get <%> target_get) (target_history ()) in
    List.take 10 % List.deduplicate <$> Lwt_list.filter_p (Option.is_some <%> Permission.can_update_private) targets
  in
  ignore
  <$> Page.open_dialog ~hide_body_overflow_y: true @@ fun return ->
    Components.Search.Quick.render
      ~return
      ~dialog_title: (lwt @@ spf "Add to %s" target_type)
      ~make_result: (make_result ~return)
      ~results_when_no_search
      quick_search

(** {!dialog} specialised for when the target is a book. *)
let dialog_to_book ~source_type ~source_format user source source_page =
  dialog
    user
    source
    ~source_type
    ~source_format
    ~target_type: "book"
    ~target_icon: Icon.(Model Book)
    ~target_format: Formatters.Book.title'
    ~target_href: Endpoints.Page.href_book
    ~target_filter_from_string: Filter.Book.from_string
    ~target_filter_owners': Filter.Book.owners'
    ~target_result: (Any_result.make_book_result ?classes: None ?prefix: None ?suffix: None)
    ~target_search: (Madge_client.call_exn Endpoints.Api.(route @@ Book Search))
    ~target_update: (Madge_client.call_exn Endpoints.Api.(route @@ Book Update))
    ~target_get: Model.Book.get
    ~target_history: History.get_books
    ~target_add_source_to_content: (fun book ->
      let%lwt contents = Model.Book.contents book in
      lwt @@ Model.Book.set_contents (contents @ [source_page]) book
    )

let button ~target_type create_dialog =
  match%lwt Environment.user with
  | None -> lwt_nil
  | Some user ->
    lwt [
      Button.make
        ~label: (spf "Add to %s" target_type)
        ~label_processing: (spf "Adding to %s..." target_type)
        ~icon: (Action Add)
        ~dropdown: true
        ~onclick: (fun () -> create_dialog user)
        ()
    ]

let button_to_book ~source_type ~source_format source source_page =
  button ~target_type: "book" (fun user -> dialog_to_book ~source_type ~source_format user source source_page)
