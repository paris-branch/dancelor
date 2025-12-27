let namespace = "dancelor"

let http_requests_total =
  Prometheus.Counter.v
    ~namespace
    ~help: "Total number of HTTP requests"
    "http_requests_total"

let increment_http_requests_total () = Prometheus.Counter.inc_one http_requests_total
