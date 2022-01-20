open Nes
open AnyLifted
include Dancelor_common_model.AnyFilter

let accepts filter any =
  Formula.interpret filter @@ function
  | Type type_ ->
    Type.equal (type_of any) type_
    |> Formula.interpret_bool
    |> Lwt.return

  | AsCredit cfilter ->
    (match any with
     | Credit credit -> CreditFilter.accepts cfilter credit
     | _ -> Lwt.return Formula.interpret_false)

  | AsDance dfilter ->
    (match any with
     | Dance dance -> DanceFilter.accepts dfilter dance
     | _ -> Lwt.return Formula.interpret_false)

  | AsPerson pfilter ->
    (match any with
     | Person person -> PersonFilter.accepts pfilter person
     | _ -> Lwt.return Formula.interpret_false)

  | AsBook bfilter ->
    (match any with
     | Book book -> BookFilter.accepts bfilter book
     | _ -> Lwt.return Formula.interpret_false)

  | AsSet sfilter ->
    (match any with
     | Set set -> SetFilter.accepts sfilter set
     | _ -> Lwt.return Formula.interpret_false)

  (* | AsSource sfilter -> *)
  (*   (match any with *)
  (*    | Source source -> SourceFilter.accepts sfilter source *)
  (*    | _ -> Lwt.return Formula.interpret_false) *)

  | AsTune tfilter ->
    (match any with
     | Tune tune -> TuneFilter.accepts tfilter tune
     | _ -> Lwt.return Formula.interpret_false)

  | AsVersion vfilter ->
    (match any with
     | Version version -> VersionFilter.accepts vfilter version
     | _ -> Lwt.return Formula.interpret_false)
