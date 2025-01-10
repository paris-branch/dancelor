open Dancelor_common_model

include VersionLifter.Lift(Person)(Tune)

let content = Dancelor_server_database.Version.read_content

let get = Dancelor_server_database.Version.get
