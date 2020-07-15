include Lwt_unix

let with_difftimeofday_gen f =
  let start = Unix.gettimeofday () in
  let%lwt v = f () in
  Lwt.return (v, Unix.gettimeofday () -. start)

let with_difftimeofday f =
  let%lwt ((), t) = with_difftimeofday_gen f in
  Lwt.return t
