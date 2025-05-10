open Nes

module Build
  (Source : Signature.Source.S)
  (Person : Signature.Person.S)
  (Tune : Signature.Tune.S)
= struct
  include Core.Version

  let make
      ~tune
      ~bars
      ~key
      ~structure
      ?sources
      ?arrangers
      ?remark
      ?disambiguation
      ~content
      ()
    =
    let structure = String.remove_duplicates ~char: ' ' structure in
    let disambiguation = Option.map (String.remove_duplicates ~char: ' ') disambiguation in
    let tune = Entry.slug tune in
    let sources = Option.map (List.map Entry.slug) sources in
    let arrangers = Option.map (List.map Entry.slug) arrangers in
    make ~tune ~bars ~key ~structure ?sources ?arrangers ?remark ?disambiguation ~content ()

  let tune = Tune.get % tune
  let tune' = tune % Entry.value

  let sources = Lwt_list.map_p Source.get % sources
  let sources' = sources % Entry.value

  let arrangers = Lwt_list.map_p Person.get % arrangers
  let arrangers' = arrangers % Entry.value

  let kind version =
    Fun.flip Lwt.map (tune version) @@ fun tune ->
    (bars version, Tune.kind' tune)
  let kind' = kind % Entry.value

  let name version = Lwt.map Tune.name' (tune version)
  let name' = name % Entry.value

  module Filter = struct
    let versionCore_tune = tune'

    include Filter.Version

    let accepts filter version =
      Formula.interpret filter @@ function
        | Is version' ->
          Lwt.return @@ Formula.interpret_bool @@ Slug.equal' (Entry.slug version) version'
        | Tune tfilter ->
          let%lwt tune = versionCore_tune version in
          Tune.Filter.accepts tfilter tune
        | Key key' ->
          Lwt.return @@ Formula.interpret_bool (Core.Version.key' version = key')
        | Kind kfilter ->
          let%lwt tune = versionCore_tune version in
          Kind.Version.Filter.accepts kfilter (Core.Version.bars' version, Tune.kind' tune)
        | ExistsSource sfilter ->
          let%lwt sources = sources' version in
          Formula.interpret_exists (Source.Filter.accepts sfilter) sources
  end
end
