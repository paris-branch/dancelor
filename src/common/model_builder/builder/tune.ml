open Nes

module Build (Getters : Getters.S) = struct
  include Core.Tune

  let get = Getters.get_tune

  let composer_core_to_composer : composer_core -> composer Lwt.t = fun {composer; details} ->
    let%lwt composer = Option.get <$> Getters.get_person composer in
    lwt {composer; details}

  let composers = Lwt_list.map_p composer_core_to_composer % composers
  let composers' = composers % Entry.value_public

  let dances = Lwt_list.map_p (Lwt.map Option.get % Getters.get_dance) % dances
  let dances' = dances % Entry.value_public
end
