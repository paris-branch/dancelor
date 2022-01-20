open Nes

module Make (Person : module type of PersonSignature) = struct
  include CreditCore

  let persons = persons >=>| Lwt_list.map_p Person.get
end
