open Nes

module Sqlgg_io : Sqlgg_io.M with type 'a future = 'a Lwt.t = struct
  type 'a future = 'a Lwt.t
  let return = Lwt.return
  let (>>=) = Lwt.bind
  let bracket x fin f =
    Lwt.bind x (fun x -> Lwt.finalize (fun () -> f x) (fun () -> fin x))
end

module Mariadb : Mariadb.Nonblocking.S with type 'a future = 'a Lwt.t = struct
  (* FIXME: I don't understand why this is needed? Maybe try updating `mariadb` in nixpkgs. *)
  type 'a future = 'a Lwt.t

  include Mariadb.Nonblocking.Make(struct
    module IO = Sqlgg_io

    let wait conn status =
      let module M = Mariadb.Nonblocking in
      (* REVIEW: This registers the [fd] in Lwt's event loop every time the
         function is called, so we might want something smarter. *)
      let fd = Lwt_unix.of_unix_file_descr @@ M.fd conn in
      let waits =
        let wait_read = if M.Status.read status then [Lwt_unix.wait_read fd] else [] in
        let wait_write = if M.Status.write status then [Lwt_unix.wait_write fd] else [] in
        let wait_timeout =
          if M.Status.timeout status then
            let ms = M.timeout_ms conn in
            (* REVIEW: [lwt_unit], really? Shouldn't we just raise a timeout immediately? *)
            [if ms <= 0 then lwt_unit else Lwt_unix.timeout (float_of_int ms /. 1000.)]
          else
              []
        in
        wait_read @ wait_write @ wait_timeout
      in
      (* REVIEW: This function doesn't handle the [except] case. Basically, we
         would want [Unix.select], but that's not really the spirit of Lwt. *)
      assert (waits <> []);
      Lwt.catch
        (fun () ->
          Lwt.map
            (fun _ ->
              M.Status.create
                ~read: (Lwt_unix.readable fd)
                ~write: (Lwt_unix.writable fd)
                ()
            )
            (Lwt.choose waits)
        )
        (function
          | Lwt_unix.Timeout -> lwt @@ M.Status.create ~timeout: true ()
          | exn -> Lwt.reraise exn
        )
  end)
end

include Sqlgg_mariadb.Default(Sqlgg_io)(Mariadb)
