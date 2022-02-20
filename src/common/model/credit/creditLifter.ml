open Nes

module Lift
    (Person : module type of PersonSignature)
= struct
  include CreditCore

  let persons credit =
    let%lwt person_slugs = persons credit in
    Lwt_list.map_p Person.get person_slugs
end
