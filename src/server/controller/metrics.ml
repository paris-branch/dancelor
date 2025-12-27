let start_time = Unix.gettimeofday ()

let namespace = "dancelor"

let uptime_seconds =
  Prometheus.Gauge.v
    ~namespace
    ~help: "Time since application started in seconds"
    "uptime_seconds"

let http_requests_total =
  Prometheus.Counter.v
    ~namespace
    ~help: "Total number of HTTP requests"
    "http_requests_total"

let increment_http_requests_total () = Prometheus.Counter.inc_one http_requests_total

let get () =
  Prometheus.Gauge.set uptime_seconds (Unix.gettimeofday () -. start_time);
  let%lwt metrics = Prometheus.CollectorRegistry.(collect default) in
  let body = Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output metrics in
  Madge_server.respond_string ~content_type: "text/plain; version=0.0.4" body
