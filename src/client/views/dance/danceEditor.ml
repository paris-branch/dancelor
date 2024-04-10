open Nes
open Js_of_ocaml
open Dancelor_client_components
open Dancelor_client_html
module Model = Dancelor_client_model
module SCDDB = Dancelor_common.SCDDB
module PageRouter = Dancelor_common.PageRouter
open Dancelor_client_utils
module Formatters = Dancelor_client_formatters

module State = struct
  type t = {
    name : string Input.Text.t;
    kind : Model.Kind.Dance.t Input.Text.t;
    devisers : Model.Person.t ListSelector.t;
    date : PartialDate.t option Input.Text.t;
    disambiguation : string option Input.Text.t;
    two_chords : (bool, string) Result.t Choices.t;
    scddb_id : SCDDB.entry_id option Input.Text.t;
  }

  let create () =
    let name = Input.Text.make @@ fun name ->
      if name = "" then Error "The name cannot be empty."
      else Ok name
    in
    let kind = Input.Text.make @@ fun kind ->
      match Model.Kind.Dance.of_string_opt kind with
      | None -> Error "Not a valid kind"
      | Some kind -> Ok kind
    in
    let devisers = ListSelector.make
        ~search: (fun slice input ->
            let threshold = 0.4 in
            let%rlwt filter = Lwt.return (Model.Person.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Person.search ~threshold ~slice filter
          )
        ~make_result: AnyResultNewAPI.make_person_result'
        Result.ok
    in
    let date = Input.Text.make @@ fun date ->
      if date = "" then Ok None
      else
        try Ok (Some (PartialDate.from_string date))
        with _ -> Error "Not a valid date"
    in
    let disambiguation = Input.Text.make @@ fun disambiguation ->
      Ok (if disambiguation = "" then None else Some disambiguation)
    in
    let two_chords = Choices.make_radios' [
        Choices.choice' ~value:false [txt "One chord"];
        Choices.choice' ~value:true [txt "Two chords"];
      ]
        ~validate: (Option.to_result ~none:"A choice must be made")
    in
    let scddb_id = Input.Text.make @@ fun scddb_id ->
      if scddb_id = "" then
        Ok None
      else
        match int_of_string_opt scddb_id with
        | Some scddb_id -> Ok (Some scddb_id)
        | None ->
          match SCDDB.dance_from_uri scddb_id with
          | Ok scddb_id -> Ok (Some scddb_id)
          | Error msg -> Error msg
    in
    {name; kind; devisers; date; disambiguation; two_chords; scddb_id}

  let clear state =
    state.name.set "";
    state.kind.set "";
    ListSelector.clear state.devisers;
    state.date.set "";
    state.disambiguation.set "";
    (* FIXME: reset two chords *)
    state.scddb_id.set ""

  let signal state =
    S.map Result.to_option @@
    RS.bind state.name.signal @@ fun name ->
    RS.bind state.kind.signal @@ fun kind ->
    RS.bind (ListSelector.signal state.devisers) @@ fun devisers ->
    RS.bind state.date.signal @@ fun date ->
    RS.bind state.disambiguation.signal @@ fun disambiguation ->
    RS.bind (Choices.signal state.two_chords) @@ fun two_chords ->
    RS.bind state.scddb_id.signal @@ fun scddb_id ->
    RS.pure (name, kind, devisers, date, disambiguation, two_chords, scddb_id)

  let submit state =
    match S.value (signal state) with
    | None -> Lwt.return_none
    | Some (name, kind, devisers, date, disambiguation, two_chords, scddb_id) ->
      Lwt.map Option.some @@
      Model.Dance.make_and_save
        ~name
        ~kind
        ~devisers
        ~two_chords
        ?scddb_id
        ?disambiguation
        ?date
        ~modified_at: (Datetime.now ()) (* FIXME: optional argument *)
        ~created_at: (Datetime.now ()) (* FIXME: not even optional *)
        ()
end

type t =
  {
    page : Dancelor_client_elements.Page.t;
    content : Dom_html.divElement Js.t;
  }

let refresh _ = ()
let contents t = t.content
let init t = refresh t

let create ?on_save page =
  let state = State.create () in

  let document = Dancelor_client_elements.Page.document page in
  let content = Dom_html.createDiv document in

  Lwt.async (fun () ->
      document##.title := Js.string "Add a dance | Dancelor";
      Lwt.return ()
    );

  (
    Dom.appendChild content @@ To_dom.of_div @@ div [
      h2 ~a:[a_class ["title"]] [txt "Add a dance"];

      form [
        Input.Text.render state.name ~placeholder:"Name";
        Input.Text.render state.kind ~placeholder:"Kind (eg. 8x32R, 2x(16R+16S))";
        ListSelector.render state.devisers;
        Input.Text.render state.date ~placeholder:"Date of devising (eg. 2019 or 2012-03-14)";
        Choices.render state.two_chords;
        Input.Text.render state.scddb_id ~placeholder:"Strathspey database URI or id (optional)";
        Input.Text.render state.disambiguation ~placeholder:"Disambiguation";

        Button.group [
          Button.save
            ~disabled: (S.map Option.is_none (State.signal state))
            ~onclick: (fun () ->
                Fun.flip Lwt.map (State.submit state) @@ Option.iter @@ fun dance ->
                let slug = Model.Dance.slug dance in
                match on_save with
                | None -> Dom_html.window##.location##.href := Js.string (PageRouter.path_dance slug)
                | Some on_save -> on_save slug
              )
            ();
          Button.clear
            ~onclick: (fun () -> State.clear state)
            ();
        ]
      ]
    ]
  );

  {page; content}

(* let make_deviser_modal editor content page = *)
(*   let modal_bg = Html.createDiv (Page.document page) in *)
(*   let person_modal = Html.createDiv (Page.document page) in *)
(*   let interface = *)
(*     PersonEditor.create page *)
(*       ~on_save:(fun slug -> *)
(*           Page.remove_modal page modal_bg; *)
(*           Dom.removeChild content modal_bg; *)
(*           Lwt.on_success (DanceEditor.set_deviser editor slug) (fun () -> Page.refresh page)) *)
(*   in *)
(*   Dom.appendChild person_modal (PersonEditor.contents interface); *)
(*   person_modal##.classList##add (js "modal-window"); *)
(*   modal_bg##.classList##add (js "modal-background"); *)
(*   Dom.appendChild modal_bg person_modal; *)
(*   Dom.appendChild content modal_bg; *)
(*   Page.register_modal page *)
(*     ~element:modal_bg *)
(*     ~on_unfocus:(fun () -> Dom.removeChild content modal_bg; Page.remove_modal page modal_bg) *)
(*     ~on_refresh:(fun () -> PersonEditor.refresh interface) *)
(*     ~targets:[person_modal] *)

(* let make_deviser_search_result editor page deviser = *)
(*   let name = Person.name deviser in *)
(*   let slug = Person.slug deviser in *)
(*   let row = Table.Row.create *)
(*       ~on_click:(fun () -> *)
(*           Lwt.on_success *)
(*             (DanceEditor.set_deviser editor slug) *)
(*             (fun () -> Page.refresh page)) *)
(*       ~cells:[ *)
(*         Table.Cell.text ~text:(Lwt.return name) page] *)
(*       page *)
(*   in *)
(*   Lwt.return row *)

(* let create ?on_save page = *)
(*   let editor = DanceEditor.create () in *)
(*   let content = Html.createDiv (Page.document page) in *)
(*   let title = Text.Heading.h2_static ~text:(Lwt.return "Add a Dance") page in *)
(*   let form = Html.createForm (Page.document page) in *)
(*   let input_date = Inputs.Text.create *)
(*       ~placeholder:"Date of devising (eg. 2019 or 2012-03-14)" *)
(*       ~on_change:(fun date -> DanceEditor.set_date editor date) *)
(*       page *)
(*   in *)
(*   let input_scddb_id = Inputs.Text.create *)
(*       ~placeholder:"Strathspey Database link or id (optional)" *)
(*       ~on_change:(fun id -> DanceEditor.set_scddb_id editor id) *)
(*       page *)
(*   in *)

(*   let input_two_chords = Inputs.Switch.create *)
(*       ~text_after:" Two Chords?" *)
(*       ~id:"Two Chords" *)
(*       ~on_change:(fun b -> DanceEditor.set_two_chords editor b) *)
(*       page *)
(*   in *)

(*   let deviser_search = *)
(*     let main_section = *)
(*       SearchBar.Section.create *)
(*         ~default:(Table.Row.create *)
(*                     ~on_click:(fun () -> make_deviser_modal editor content page) *)
(*                     ~cells:[ *)
(*                       Table.Cell.text ~colspan:0 ~icon:"add_circle" ~text:(Lwt.return "Create a new deviser") page] *)
(*                     page) *)
(*         ~search:(fun input -> *)
(*             let%rlwt formula = Lwt.return @@ Person.Filter.from_string input in *)
(*             let%lwt results = Person.search' ~threshold:0.4 ~slice:(Slice.make ~start:0 ~end_excl:10 ()) formula in *)
(*             Lwt.return_ok results) *)
(*         ~make_result:(make_deviser_search_result editor page) *)
(*         page *)
(*     in *)
(*     SearchBar.create *)
(*       ~placeholder:"Devisor (Magic Search)" *)
(*       ~sections:[main_section] *)
(*       page *)
(*   in *)

(*   Inputs.Text.on_focus (SearchBar.bar deviser_search) (fun b -> *)
(*       if b then begin *)
(*         Inputs.Text.erase (SearchBar.bar deviser_search); *)
(*         DanceEditor.remove_deviser editor; *)
(*         Page.refresh page *)
(*       end); *)

(*   let submit = Html.createDiv (Page.document page) in *)
(*   Style.set ~display:"flex" submit; *)
(*   submit##.classList##add (js "justify-content-space-between"); *)

(*   let t = {page; editor; content; input_name; input_kind; input_date; deviser_search; input_two_chords; input_scddb_id} in *)

(*   let save = *)
(*     Inputs.Button.create ~kind:Inputs.Button.Kind.Success ~icon:"save" ~text:"Save" *)
(*       ~on_click:(fun () -> *)
(*           let b1, b2, b3, b4 = *)
(*             Inputs.Text.check input_name (fun str -> str <> ""), *)
(*             Inputs.Text.check input_kind (fun str -> try Kind.Dance.of_string str |> ignore; true with _ -> false), *)
(*             Inputs.Text.check input_date (fun str -> str = "" || try PartialDate.from_string str |> ignore; true with _ -> false), *)
(*             Inputs.Text.check input_scddb_id (fun str -> *)
(*                 if str = "" then *)
(*                   true *)
(*                 else *)
(*                   match int_of_string_opt str with *)
(*                   | Some _ -> true *)
(*                   | None -> *)
(*                     match SCDDB.dance_from_uri str with *)
(*                     | Ok _ -> true *)
(*                     | Error _ -> false *)
(*               ) *)
(*           in *)
(*           if b1 && b2 && b3 && b4 then ( *)
(*             Lwt.on_success (DanceEditor.submit editor) (fun dance -> *)
(*                 let slug = Dance.slug dance in *)
(*                 match on_save with *)
(*                 | None -> Html.window##.location##.href := js (PageRouter.path_dance slug) *)
(*                 | Some cb -> cb slug *)
(*               ))) *)
(*       page *)
(*   in *)

(*   let clear = *)
(*     Inputs.Button.create ~kind:Inputs.Button.Kind.Danger ~icon:"cancel" ~text:"Clear" *)
(*       ~on_click:(fun () -> *)
(*           if Html.window##confirm (js "Clear the editor?") |> Js.to_bool then begin *)
(*             DanceEditor.clear editor; *)
(*             Page.refresh page; *)
(*             Inputs.Text.set_valid input_name true; *)
(*             Inputs.Text.set_valid input_kind true; *)
(*             Inputs.Text.set_valid input_date true; *)
(*             Inputs.Text.set_valid input_scddb_id true *)
(*           end) *)
(*       page *)
(*   in *)

(*   Dom.appendChild submit (Inputs.Button.root save); *)
(*   Dom.appendChild submit (Inputs.Button.root clear); *)

(*   Dom.appendChild form (Inputs.Text.root input_name); *)
(*   Dom.appendChild form (Html.createBr (Page.document page)); *)
(*   Dom.appendChild form (Inputs.Text.root input_kind); *)
(*   Dom.appendChild form (Html.createBr (Page.document page)); *)
(*   Dom.appendChild form (SearchBar.root deviser_search); *)
(*   Dom.appendChild form (Html.createBr (Page.document page)); *)
(*   Dom.appendChild form (Inputs.Text.root input_date); *)
(*   Dom.appendChild form (Html.createBr (Page.document page)); *)
(*   Dom.appendChild form (Inputs.Switch.root input_two_chords); *)
(*   Dom.appendChild form (Html.createBr (Page.document page)); *)
(*   Dom.appendChild form (Html.createBr (Page.document page)); *)
(*   Dom.appendChild form (Inputs.Text.root input_scddb_id); *)
(*   Dom.appendChild form (Html.createBr (Page.document page)); *)
(*   Dom.appendChild form submit; *)

(*   Dom.appendChild content (Text.Heading.root title); *)
(*   Dom.appendChild content (Html.createHr (Page.document page)); *)
(*   Dom.appendChild content (Html.createBr (Page.document page)); *)
(*   Dom.appendChild content form; *)
(*   t *)

(* let contents t = *)
(*   t.content *)

(* let init t = *)
(*   refresh t *)
