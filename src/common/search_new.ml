open Nes

(** {2 Results} *)

module type Type = sig
  type t
  [@@deriving yojson]
end

module Search_result = struct
  type 'a t = {
    total: int;
    items: 'a list;
  }
  [@@deriving yojson, fields]

  let map f {total; items} = {total; items = List.map f items}
end

module Make_search_result (X : Type) = struct
  type t = X.t Search_result.t
  [@@deriving yojson]
end

module Search_context_result = struct
  type 'a t = {
    total: int;
    previous_item: 'a option;
    index: int;
    next_item: 'a option;
  }
  [@@deriving yojson, fields]
end

module Make_search_context_result (X : Type) = struct
  type t = X.t Search_context_result.t
  [@@deriving yojson]
end

(** {2 Queries} *)

module Query_parser = struct
  exception Parse_error of string
  let parse_errorf fmt = Format.kasprintf (fun msg -> raise (Parse_error msg)) fmt

  type operators = string list String.Map.t

  type split = {
    terms: string;
    operators: operators;
  }

  let split query : split =
    let components = String.split_on_char ' ' query in
    let (terms, operators) =
      List.partition_map
        (fun component ->
          match String.split_on_char ':' component with
          | [] -> assert false
          | [term] -> Left term
          | [operator; arguments] -> Right (operator, String.split_on_char ',' arguments)
          | _ -> parse_errorf "the component %S contains more than one column character (':')" component
        )
        components
    in
    let terms = String.concat " " terms in
    let operators =
      List.fold_left
        (fun map (operator, arguments) ->
          if String.Map.mem operator map then
            parse_errorf "the operator %S is used several times" operator
          else
            String.Map.add operator arguments map
        )
        String.Map.empty
        operators
    in
      {terms; operators}

  type parse_operator = {
    parse_operator: 'result. string -> (string list -> 'result) -> 'result option;
  }

  let make parse_operators query =
    try
      let {terms; operators} = split query in
      let remaining_operators = ref operators in
      let parse_operator = {
        parse_operator = fun op f ->
          match String.Map.find_opt op !remaining_operators with
          | None -> None
          | Some arguments ->
            remaining_operators := String.Map.remove op !remaining_operators;
            Some (f arguments)
      }
      in
      let result = parse_operators terms parse_operator in
      match String.Map.choose_opt !remaining_operators with
      | Some (operator, _arguments) -> parse_errorf "unexpected operator %S" operator
      | None -> Ok result
    with
      | Parse_error msg -> Error msg
      | exn -> Error (spf "unexpected exception while parsing: %s" @@ Printexc.to_string exn)
end

module Query = struct
  type common = {
    name: string;
  }
  [@@deriving yojson]

  type 'a t =
    {common: common; specific: 'a}
  [@@deriving yojson]

  let make_parser f =
    Query_parser.make @@ fun terms {parse_operator} ->
    let specific = f {Query_parser.parse_operator} in
      {common = {name = terms}; specific}
end

module Person_query = struct
  type specific = unit
  [@@deriving yojson]

  let no_specific = ()

  type t = specific Query.t
  [@@deriving yojson]

  let parse_operators = fun {Query_parser.parse_operator} ->
    ignore parse_operator

  let parse = Query.make_parser parse_operators
end

module User_query = struct
  type specific = unit
  [@@deriving yojson]

  let no_specific = ()

  type t = specific Query.t
  [@@deriving yojson]

  let parse_operators = fun {Query_parser.parse_operator} ->
    ignore parse_operator

  let parse = Query.make_parser parse_operators
end

module Dance_query = struct
  type specific = {
    kind: Kind_base.t list option; [@default None]
  }
  [@@deriving yojson]

  let no_specific = {
    kind = None;
  }

  type t = specific Query.t
  [@@deriving yojson]

  let parse_operators = fun {Query_parser.parse_operator} ->
    let kind = parse_operator "kind" (List.map Kind_base.of_string) in
      {kind}

  let parse = Query.make_parser parse_operators
end

module Source_query = struct
  type specific = unit
  [@@deriving yojson]

  let no_specific = ()

  type t = specific Query.t
  [@@deriving yojson]

  let parse_operators = fun {Query_parser.parse_operator} ->
    ignore parse_operator

  let parse = Query.make_parser parse_operators
end

module Tune_query = struct
  type specific = {
    kind: Kind_base.t list option; [@default None]
  }
  [@@deriving yojson]

  let no_specific = {
    kind = None;
  }

  type t = specific Query.t
  [@@deriving yojson]

  let parse_operators = fun {Query_parser.parse_operator} ->
    let kind = parse_operator "kind" (List.map Kind_base.of_string) in
      {kind}

  let parse = Query.make_parser parse_operators
end

module Version_query = struct
  type specific = {
    tune: Tune_query.specific;
    key: Music.Key.t list option; [@default None]
  }
  [@@deriving yojson]

  let no_specific = {
    tune = Tune_query.no_specific;
    key = None;
  }

  type t = specific Query.t
  [@@deriving yojson]

  let parse_operators = fun {Query_parser.parse_operator} ->
    let key = parse_operator "key" (List.map Music.Key.of_string) in
    let tune = Tune_query.parse_operators {parse_operator} in
      {tune; key}

  let parse = Query.make_parser parse_operators
end

module Set_query = struct
  type specific = {
    kind: Kind_base.t list option; [@default None]
  }
  [@@deriving yojson]

  let no_specific = {
    kind = None;
  }

  type t = specific Query.t
  [@@deriving yojson]

  let parse_operators = fun {Query_parser.parse_operator} ->
    let kind = parse_operator "kind" (List.map Kind_base.of_string) in
      {kind}

  let parse = Query.make_parser parse_operators
end

module Book_query = struct
  type specific = unit
  [@@deriving yojson]

  let no_specific = ()

  type t = specific Query.t
  [@@deriving yojson]

  let parse_operators = fun {Query_parser.parse_operator} ->
    ignore parse_operator

  let parse = Query.make_parser parse_operators
end

module Any_query = struct
  type model_specific =
    | Person of Person_query.specific
    | User of User_query.specific
    | Dance of Dance_query.specific
    | Source of Source_query.specific
    | Tune of Tune_query.specific
    | Version of Version_query.specific
    | Set of Set_query.specific
    | Book of Book_query.specific
  [@@deriving yojson]

  type specific = model_specific option
  [@@deriving yojson]

  type t = specific Query.t
  [@@deriving yojson]

  let parse : string -> (t, string) result =
    Query.make_parser @@ fun {parse_operator} ->
    match parse_operator "type" (List.map String.lowercase_ascii) with
    | None -> None
    | Some type_ ->
      some @@
        match type_ with
        | ["person"] -> Person (Person_query.parse_operators {parse_operator})
        | ["user"] -> User (User_query.parse_operators {parse_operator})
        | ["dance"] -> Dance (Dance_query.parse_operators {parse_operator})
        | ["source"] -> Source (Source_query.parse_operators {parse_operator})
        | ["tune"] -> Tune (Tune_query.parse_operators {parse_operator})
        | ["version"] -> Version (Version_query.parse_operators {parse_operator})
        | ["set"] -> Set (Set_query.parse_operators {parse_operator})
        | ["book"] -> Book (Book_query.parse_operators {parse_operator})
        | _ -> Query_parser.parse_errorf "unexpected type %S" (String.concat "," type_)
end
