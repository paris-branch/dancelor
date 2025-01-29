open Nes

module Build
    (Dance : Signature.Dance.S)
    (Person : Signature.Person.S)
= struct
  include Core.Tune

  let make
      ~name
      ?alternative_names
      ~kind
      ?composers
      ?dances
      ?remark
      ?scddb_id
      ?date
      ()
    =
    let name = String.remove_duplicates ~char: ' ' name in
    let alternative_names = Option.map (List.map (String.remove_duplicates ~char: ' ')) alternative_names in
    let composers = Option.map (List.map Entry.slug) composers in
    let dances = Option.map (List.map Entry.slug) dances in
    make ~name ?alternative_names ~kind ?composers ?dances ?remark ~scddb_id ~date ()

  let composers = Lwt_list.map_p Person.get % composers
  let dances = Lwt_list.map_p Dance.get % dances

  module Filter = struct
    (* NOTE: [include Core.Tune.Filter] shadows the accessors of [Dancelor_common.Model_core.Tune]. *)
    let tuneCore_dances = dances

    include Filter.Tune

    let accepts filter tune =
      let char_equal = Char.Sensible.equal in
      Formula.interpret filter @@ function
      | Is tune' ->
        Lwt.return @@ Formula.interpret_bool @@ Slug.equal' (Entry.slug tune) tune'
      | Name string ->
        Lwt.return @@ String.proximity ~char_equal string @@ Core.Tune.name tune
      | NameMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle: string @@ Core.Tune.name tune
      | ExistsComposer pfilter ->
        let%lwt composers = composers tune in
        let%lwt scores = Lwt_list.map_s (Person.Filter.accepts pfilter) composers in
        Lwt.return (Formula.interpret_or_l scores)
      | Kind kfilter ->
        Kind.Base.Filter.accepts kfilter @@ Core.Tune.kind tune
      | ExistsDance dfilter ->
        let%lwt dances = tuneCore_dances tune in
        let%lwt scores = Lwt_list.map_s (Dance.Filter.accepts dfilter) dances in
        Lwt.return (Formula.interpret_or_l scores)
  end
end
