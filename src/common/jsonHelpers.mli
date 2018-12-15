val check_object : Ezjsonm.value -> [`O of (string * Ezjsonm.value) list]
val add_field : string -> Ezjsonm.value -> Ezjsonm.value -> Ezjsonm.value
val add_fields : (string * Ezjsonm.value) list -> Ezjsonm.value -> Ezjsonm.value
val remove_field : string -> Ezjsonm.value -> Ezjsonm.value
