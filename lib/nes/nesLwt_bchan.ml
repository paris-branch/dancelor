type 'a port = {
  mutable pvalue : 'a;
  mutable promise : unit Lwt.t;
  mutable resolver : unit Lwt.u;
}

type 'a t = {
  mutable value : 'a;
  mutable ports : 'a port list;
}

let create value = { value; ports = [] }

let put bvar value =
  bvar.value <- value;
  List.iter
    (fun port ->
       port.pvalue <- value;
       if Lwt.is_sleeping port.promise then
         Lwt.wakeup_later port.resolver ())
    bvar.ports

let create_port bvar =
  let (promise, resolver) = Lwt.wait () in
  Lwt.wakeup_later resolver ();
  let port = {
    pvalue = bvar.value;
    promise;
    resolver;
  }
  in
  bvar.ports <- port :: bvar.ports;
  port

let take port =
  Lwt.bind port.promise @@ fun () ->
  let (new_promise, new_resolver) = Lwt.wait () in
  port.promise <- new_promise;
  port.resolver <- new_resolver;
  Lwt.return port.pvalue
