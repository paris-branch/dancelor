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
  | Ok jobId ->
    lwt @@
    Lwt_stream.from_next @@ fun () ->
    Js_of_ocaml_lwt.Lwt_js.sleep 3.;%lwt
    let%lwt status = Madge_client.call_exn Endpoints.Api.(route @@ Job Status) jobId in
    lwt @@
      match status with
      | Pending -> Lwt_stream.Next Pending
      | Running logs -> Lwt_stream.Next (Running logs)
      | Failed logs -> Lwt_stream.Next (Failed logs)
      | Succeeded -> Lwt_stream.Last (Succeeded (Endpoints.Api.(href @@ Job File) jobId slug))

let status_signal_from_promise = S.switch % S.from' (S.const Registering)

let show_logs logs = pre [small [txt (String.concat "\n" logs)]]

let add_spinner content =
  div ~a: [a_class ["position-relative"]] [
    div ~a: [a_class ["opacity-50"]; a_style "min-height: 5em;"] [content];
    div ~a: [a_class ["position-absolute top-50 start-50 translate-middle"]] [
      div ~a: [a_class ["spinner-border"]; a_role ["status"]] [];
    ]
  ]

let show_live_status ~on_succeeded status_signal =
  flip S.map status_signal @@ function
    | Registering ->
      [
        Utils.Alert.make ~level: Info ~icon: CloudUpload [
          txt
            "The document generation job is being sent to the server.";
        ];
        div ~a: [a_class ["mt-4"]] [add_spinner (show_logs [])];
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
        div ~a: [a_class ["mt-4"]] [add_spinner (show_logs [])];
      ]
    | Running logs ->
      [
        Utils.Alert.make ~level: Info ~icon: Cpu [
          txt
            "The server has started generating the document. This process can be \
           short for single tunes, but can also take a (very) long time, up to \
           several minutes, for big books. Go get yourself a tea.";
        ];
        div ~a: [a_class ["mt-4"]] [add_spinner (show_logs logs)];
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

let show_placeholder ~on_succeeded status_signal =
  flip S.map status_signal @@ function
    | Succeeded href -> on_succeeded href
    | _ -> [div_placeholder ~min: 12 ~max: 20 ()]
