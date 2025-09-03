open Js_of_ocaml
open Nes
open Common
open Html

(** Similar to {!Endpoints.Job.Status.t} but with an additional “Registering”
    and with “Succeeded” carrying the path. *)
type status =
  | Registering
  | Pending
  | Running of string list
  | Failed of string list
  | Succeeded of string
[@@deriving yojson]

(** Run a job, and return a signal that contains its status. *)
let run slug route =
  Madge_client.call_gen route @@ fun promise ->
  S.from_lwt_stream Registering @@
  Lwt_stream.(concat % return_lwt) @@
  match%lwt promise with
  | Error error -> raise (Madge_client.Error error)
  | Ok Endpoints.Job.Registration.AlreadySucceeded jobId ->
    lwt @@ Lwt_stream.return (Succeeded (Endpoints.Api.(href @@ Job File) jobId slug))
  | Ok Endpoints.Job.Registration.Registered jobId ->
    let first_time = ref true in
    lwt @@
    Lwt_stream.from_next @@ fun () ->
    (* the very first time, do not wait *)
    if !first_time then (first_time := false; lwt_unit) else Js_of_ocaml_lwt.Lwt_js.sleep 3.;%lwt
    let%lwt status = Madge_client.call_exn Endpoints.Api.(route @@ Job Status) jobId in
    lwt @@
      match status with
      | Pending -> Lwt_stream.Next Pending
      | Running logs -> Lwt_stream.Next (Running logs)
      | Failed logs -> Lwt_stream.Next (Failed logs)
      | Succeeded -> Lwt_stream.Last (Succeeded (Endpoints.Api.(href @@ Job File) jobId slug))

let status_signal_from_promise = S.switch % S.from' (S.const Registering)

let show_logs logs = pre ~a: [a_style "white-space: pre-wrap;"] [small [txt (String.concat "\n" logs)]]

let spinner () =
  let spinner =
    div ~a: [a_class ["d-flex"; "justify-content-center"; "pb-4"]] [
      div ~a: [a_class ["spinner-border"]; a_role ["status"]] [];
    ]
  in
  let spinner_dom = To_dom.of_div spinner in
  Lwt.async (fun () ->
    Js_of_ocaml_lwt.Lwt_js.sleep 0.1;%lwt
    spinner_dom##scrollIntoView Js._false;
    lwt_unit
  );
  spinner

let show_live_status ~on_succeeded status_signal =
  flip S.map status_signal @@ function
    | Registering ->
      [
        Utils.Alert.make ~level: Info ~icon: CloudUpload [
          txt
            "The document generation job is being sent to the server.";
        ];
        div ~a: [a_class ["mt-4"]] [show_logs []; spinner ()];
      ]
    | Pending ->
      [
        Utils.Alert.make
          ~level: Info
          ~icon: HourglassBottom
          [
            txt
              "The document generation job is pending, that is it has been \
             registered on the server, but the server is busy with other jobs. \
             Go get yourself a tea.";
          ];
        div ~a: [a_class ["mt-4"]] [show_logs []; spinner ()];
      ]
    | Running logs ->
      [
        Utils.Alert.make ~level: Info ~icon: Cpu [
          txt
            "The server has started generating the document. This process can be \
           short for single tunes, but can also take a (very) long time, up to \
           several minutes, for big books. Go get yourself a tea.";
        ];
        div ~a: [a_class ["mt-4"]] [show_logs logs; spinner ()];
      ]
    | Failed logs ->
      [
        Utils.Alert.make ~level: Danger [
          txt
            "There was a problem during document generation, presumably because \
           the LilyPond of a tune is erroneous. Fix the error, or report an \
           issue.";
        ];
        div ~a: [a_class ["mt-4"]] [show_logs logs];
      ]
    | Succeeded href -> on_succeeded href

(** An intermediary status conflating all the “wait” status. This is useful in
    signal, eg. to avoid placeholders flickering on irrelevant changes *)
type wait_status =
  | Waiting
  | Failed
  | Succeeded of string

let status_to_wait_status : status -> wait_status = function
  | Succeeded href -> Succeeded href
  | Failed _ -> Failed
  | _ -> Waiting

(** Variant of {!job_live_status} that only shows a placeholder on all waiting
    statuses. It is meant to be used in places where people should not be
    exposed to logs. *)
let show_placeholder ~on_succeeded status_signal =
  flip S.map (S.map status_to_wait_status status_signal) @@ function
    | Waiting -> [div_placeholder ~min: 12 ~max: 20 ()]
    | Failed ->
      [
        Utils.Alert.make ~level: Danger [
          txt
            "There was a problem during document generation, presumably because \
           the LilyPond of a tune is erroneous. Fix the error, or report an \
           issue.";
        ];
      ]
    | Succeeded href -> on_succeeded href
