open NesUnix

module Json = struct
  include Json

  (* NOTE: We are building a [from_string] that understands all of
     [Yojson.Safe.t] but also if under a YAML format. *)

  let of_yaml_scalar (sc : Yaml.scalar) : t =
    match sc.style with
    | `Any -> invalid_arg "NesJson.of_yaml_scalar: unsupported scalar type: `Any"
    | `Folded -> invalid_arg "NesJson.of_yaml_scalar: unsupported scalar type: `Folded"
    | `Literal | `Single_quoted | `Double_quoted -> `String sc.value
    | `Plain ->
      match int_of_string_opt sc.value with
      | Some i -> `Int i
      | None ->
        match float_of_string_opt sc.value with
        | Some f -> `Float f
        | None ->
          if sc.value = "true" then `Bool true
          else if sc.value = "false" then `Bool false
          else `String sc.value

  let string_of_yaml : Yaml.yaml -> string = function
    | `Scalar sc -> sc.value
    | _ -> invalid_arg "NesJson.string_of_yaml: a non-scalar cannot be a string"

  let rec from_yaml : Yaml.yaml -> t = function
    | `Scalar sc -> of_yaml_scalar sc
    | `Alias _ -> invalid_arg "NesJson.of_yaml: unsupported aliases"
    | `A sequence -> `List (List.map from_yaml sequence.s_members)
    | `O mapping -> `Assoc (List.map (fun (k, v) -> (string_of_yaml k, from_yaml v)) mapping.m_members)

  let from_yaml_string str = str |> Yaml.yaml_of_string |> Result.get_ok |> from_yaml

  (* NOTE: `Yaml.value` is in fact the JSON subset of YAML as represented by the
     `yaml` library. This is more than enough for our use. *)
  let rec basic_to_yaml : Yojson.Basic.t -> Yaml.value = function
    | `Null -> `Null
    | `Bool b -> `Bool b
    | `Int i -> `Float (float_of_int i)
    | `Float f -> `Float f
    | `String s -> `String s
    | `List values -> `A (List.map basic_to_yaml values)
    | `Assoc bindings -> `O (List.map (fun (k, v) -> (k, basic_to_yaml v)) bindings)

  let to_yaml json = basic_to_yaml (Yojson.Safe.to_basic json)

  let to_yaml_string json = json |> to_yaml |> Yaml.to_string_exn
end
