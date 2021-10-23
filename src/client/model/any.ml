open Nes
include Dancelor_common_model.Any

module Filter = struct
  include Filter

  let accepts filter any =
    Formula.interpret filter @@ function
    | Type type_ ->
      Type.equal (type_of any) type_
      |> Formula.interpret_bool
      |> Lwt.return

    | AsCredit cfilter ->
      (match any with
       | Credit credit -> Credit.Filter.accepts cfilter credit
       | _ -> Lwt.return Formula.interpret_false)

    | AsDance dfilter ->
      (match any with
       | Dance dance -> Dance.Filter.accepts dfilter dance
       | _ -> Lwt.return Formula.interpret_false)

    | AsPerson pfilter ->
      (match any with
       | Person person -> Person.Filter.accepts pfilter person
       | _ -> Lwt.return Formula.interpret_false)

    | AsBook bfilter ->
      (match any with
       | Book book -> Book.Filter.accepts bfilter book
       | _ -> Lwt.return Formula.interpret_false)

    | AsSet sfilter ->
      (match any with
       | Set set -> Set.Filter.accepts sfilter set
       | _ -> Lwt.return Formula.interpret_false)

    (* | AsSource sfilter -> *)
    (*   (match any with *)
    (*    | Source source -> Source.Filter.accepts sfilter source *)
    (*    | _ -> Lwt.return Formula.interpret_false) *)

    | AsTune tfilter ->
      (match any with
       | Tune tune -> Tune.Filter.accepts tfilter tune
       | _ -> Lwt.return Formula.interpret_false)

    | AsVersion vfilter ->
      (match any with
       | Version version -> Version.Filter.accepts vfilter version
       | _ -> Lwt.return Formula.interpret_false)
end

module E = Dancelor_common_model.Any_endpoints
module A = E.Arguments

let search ?pagination ?threshold filter =
  Madge_client.(
    call ~endpoint:E.search @@ fun {a} {o} ->
    o A.pagination pagination;
    o A.threshold  threshold;
    a A.filter     filter
  )
