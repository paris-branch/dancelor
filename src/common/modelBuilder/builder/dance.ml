open Nes

module Build
  (Person : Signature.Person.S)
= struct
  include Core.Dance

  let make
      ~name
      ~kind
      ?devisers
      ?two_chords
      ?scddb_id
      ?disambiguation
      ?date
      ()
    =
    let name = String.remove_duplicates ~char: ' ' name in
    let disambiguation = Option.map (String.remove_duplicates ~char: ' ') disambiguation in
    let devisers = Option.map (List.map Entry.slug) devisers in
    make ~name ~kind ?devisers ~two_chords ~scddb_id ?disambiguation ~date ()

  let devisers = Lwt_list.map_p Person.get % devisers

  let equal dance1 dance2 = Slug.equal' (Entry.slug dance1) (Entry.slug dance2)

  module Filter = struct
    include Filter.Dance

    let accepts filter dance =
      let char_equal = Char.Sensible.equal in
      Formula.interpret filter @@ function
        | Is dance' ->
          Lwt.return @@ Formula.interpret_bool @@ Slug.equal' (Entry.slug dance) dance'
        | Name string ->
          Lwt.return @@ String.proximity ~char_equal string @@ Core.Dance.name dance
        | NameMatches string ->
          Lwt.return @@ String.inclusion_proximity ~char_equal ~needle: string @@ Core.Dance.name dance
        | Kind kfilter ->
          Kind.Dance.Filter.accepts kfilter @@ Core.Dance.kind dance
        | ExistsDeviser pfilter ->
          let%lwt devisers = devisers dance in
          let%lwt scores = Lwt_list.map_s (Person.Filter.accepts pfilter) devisers in
          Lwt.return (Formula.interpret_or_l scores)
  end
end
