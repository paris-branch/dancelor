let spf = Format.sprintf

let debug s =
  Logs_lwt.debug (fun m -> m "%s" s)

let debug_async s =
  Lwt.async (fun () -> debug s)

let warning s =
  Logs_lwt.warn (fun m -> m "%s" s)

let warning_async s =
  Lwt.async (fun () -> warning s)

let error s =
  Logs_lwt.err (fun m -> m "%s" s)

let error_async s =
  Lwt.async (fun () -> error s)

(*FIXME!*)

let () = Logs.(set_level ~all:true (Some Debug))
let () = Logs.(set_reporter (format_reporter ()))
