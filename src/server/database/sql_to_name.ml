open Dancelor_common
open Model_new

let person_sql_to_name ~id ~name ~(k : Person_name.t -> 'w) : 'w =
  k {id = Entry.Id.of_string_exn id; name}

let person_sql_to_name_with_details ~id ~name ~details ~(k : Person_name_with_details.t -> 'w) : 'w =
  k {id = Entry.Id.of_string_exn id; name; details}

let source_sql_to_name ~id ~name ~(k : Source_name.t -> 'w) : 'w =
  k {id = Entry.Id.of_string_exn id; name}

let source_sql_to_short_name ~id ~name ~short_name ~(k : Source_short_name.t -> 'w) : 'w =
  let short_name = Option.value short_name ~default: name in
  k {id = Entry.Id.of_string_exn id; short_name}

let version_sql_to_name ~id ~name ~(k : Version_name.t -> 'w) : 'w =
  k {id = Entry.Id.of_string_exn id; name}
