open Nes
open Common
open Html

type status =
  | Registering (** we have sent the request to the server *)
  | Pending (** the server has processed our request *)
  | Running of string list (** the server is running the job; carries the output *)
  | Failed of string list (** the job has failed; carries the output *)
  | Succeeded of string (** the job has succeeded; carries the link of the file *)

(** Run a job, and return a signal that contains its status. *)
let run slug route =
  Madge_client.call_gen route @@ function
    | Error error -> raise (Madge_client.Error error)
    | Ok jobId ->
      lwt @@
      S.from_lwt_stream Pending @@
      Lwt_stream.concat @@
      Lwt_stream.return_lwt @@
      lwt @@
      Lwt_stream.from_next @@ fun () ->
      Js_of_ocaml_lwt.Lwt_js.sleep 3.;%lwt
      let%lwt status = Madge_client.call_exn Endpoints.Api.(route @@ Job Status) jobId in
      lwt @@
        match status with
        | {status = Pending; _} -> Lwt_stream.Next Pending
        | {status = Running; log_lines; _} -> Lwt_stream.Next (Running log_lines)
        | {status = Failed; log_lines; _} -> Lwt_stream.Next (Failed log_lines)
        | {status = Succeeded; _} -> Lwt_stream.Last (Succeeded (Endpoints.Api.(href @@ Job File) jobId slug))

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
        p ~a: [a_class ["mb-4"]] [
          txt
            "The document generation job is being sent to the server.";
        ];
        add_spinner (show_logs []);
      ]
    | Pending ->
      [
        p ~a: [a_class ["mb-4"]] [
          txt
            "The document generation job is pending, that is it has been \
           registered on the server, but the server is busy with other jobs.";
        ];
        add_spinner (show_logs []);
      ]
    | Running log_lines ->
      [
        p ~a: [a_class ["mb-4"]] [
          txt
            "The server has started generating the document. This process can take \
           a (very) long time, up to several minutes. Wait until you get \
           redirected, or until an error message shows.";
        ];
        add_spinner (show_logs log_lines);
      ]
    | Failed log_lines ->
      [
        p ~a: [a_class ["mb-4"]] [
          txt
            "There was a problem during document generation. This is not your \
           fault. You may try again, but if it continues, contact your system \
           administrator. Give them the logs below.";
        ];
        show_logs log_lines;
      ]
    | Succeeded href -> on_succeeded href

(* S.from' [div_placeholder ~min: 12 ~max: 20 ()] @@ *)
