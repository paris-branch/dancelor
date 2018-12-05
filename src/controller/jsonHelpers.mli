type json = Ezjsonm.value
type json_object = [`O of (string * Ezjsonm.value) list]

val add_field : string -> json -> json -> json
