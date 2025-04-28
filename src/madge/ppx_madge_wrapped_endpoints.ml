open Ppxlib
open Ast_builder.Default

(** Helper to make a list expression [\[a; b; ... c\]] from a list of
    expressions [a], [b], ..., [c]. *)
let rec pexp_list ~loc = function
  | [] -> pexp_construct ~loc (Loc.make ~loc @@ lident "[]") None
  | x :: xs ->
    pexp_construct
      ~loc
      (Loc.make ~loc @@ lident "::")
      (Some (pexp_tuple ~loc [x; pexp_list ~loc xs]))

let loc_lident ~loc txt = Loc.make ~loc @@ Longident.parse txt

let wrapped_name ~quot typ =
  (match typ with "t" -> "wrapped" | _ -> "wrapped_" ^ typ) ^
    (if quot then "'" else "")

let w_name ~quot typ =
  (match typ with "t" -> "W" | _ -> "W_" ^ typ) ^
    (if quot then "'" else "")

let all_names ~quot typ =
  (match typ with "t" -> "all" | _ -> "all_" ^ typ ^ "s") ^
    (if quot then "'" else "")

(** Generates one of:

        type wrapped_<name> = W_<name> : ('a, 'r Lwt.t, 'r) <name> -> wrapped_<name>

    or

        type 'w wrapped_<name>' = W_<name>' : ('a, 'w, 'r) <name> -> 'w wrapped_<name>'

    depending on the [~quot] argument. *)
let generate_type_wrapped ~quot ~loc name =
  pstr_type ~loc Recursive [
    type_declaration
      ~loc
      ~name: (Loc.make ~loc @@ wrapped_name ~quot name)
      ~params: (if quot then [(ptyp_var ~loc "w", (NoVariance, NoInjectivity))] else [])
      ~cstrs: []
      ~kind: (
        (* W_<name> : ('a, 'r Lwt.t, 'r) <name> -> wrapped_<name> *)
        (* W_<name>' : ('a, 'w, 'r) <name> -> 'w wrapped_<name>' *)
        Ptype_variant [
          constructor_declaration
            ~loc
            ~name: (Loc.make ~loc @@ w_name ~quot name)
            ~args: (
              Pcstr_tuple [
                (* ('a, 'r Lwt.t, 'r) <name> *)
                (* ('a, 'w, 'r) <name> *)
                ptyp_constr
                  ~loc
                  (Loc.make ~loc @@ Lident name)
                  [
                    ptyp_var ~loc "a";
                    (
                      if quot then
                        ptyp_var ~loc "w"
                      else
                        ptyp_constr ~loc (loc_lident ~loc "Lwt.t") [ptyp_var ~loc "r"]
                    );
                    ptyp_var ~loc "r";
                  ]
              ]
            )
            ~res: (
              Some (
                (* wrapped_<name> *)
                (* 'w wrapped_<name>' *)
                ptyp_constr
                  ~loc
                  (loc_lident ~loc @@ wrapped_name ~quot name)
                  (if quot then [ptyp_var ~loc "w"] else [])
              )
            )
        ]
      )
      ~private_: Public
      ~manifest: None
  ]

(** Generates one of

        let all_<name>s : wrapped list = [W_<name> <cstr1>; W_<name> <cstr2>]

    or

        let all_<name>s' : 'w wrapped' list = [W_<name>' <cstr1>; W_<name>' <cstr2>]

    depending on the [~quot] argument. *)
let generate_all_names ~quot ~loc name cds =
  pstr_value ~loc Nonrecursive [
    value_binding
      ~loc
      ~pat: (
        (* all_<name>s : wrapped list *)
        (* all_<name>s' : 'w wrapped' list *)
        ppat_constraint
          ~loc
          (ppat_var ~loc @@ Loc.make ~loc @@ all_names ~quot name)
          (
            ptyp_constr ~loc (loc_lident ~loc "list") [
              ptyp_constr
                ~loc
                (loc_lident ~loc @@ wrapped_name ~quot name)
                (if quot then [ptyp_var ~loc "w"] else []);
            ]
          )
      )
      ~expr: (
        (* [W_<name> <cstr1>; W_<name> <cstr2>] *)
        (* [W_<name>' <cstr1>; W_<name>' <cstr2>] *)
        pexp_list ~loc (
          List.map
            (fun cd ->
              match cd.pcd_args with
              | Pcstr_tuple [] ->
                (* W_<name> <cstr> *)
                (* W_<name>' <cstr> *)
                pexp_construct
                  ~loc
                  (loc_lident ~loc @@ w_name ~quot name)
                  (Some (pexp_construct ~loc (loc_lident ~loc cd.pcd_name.txt) None))
              | Pcstr_tuple [_] -> assert false (* FIXME *)
              | _ ->
                Location.raise_errorf
                  ~loc: cd.pcd_loc
                  "ppx_madge_wrapped_endpoints: constructor with arguments not supported"
            )
            cds
        )
      )
  ]

let generate ~loc name cds = [
  generate_type_wrapped ~quot: false ~loc name;
  generate_type_wrapped ~quot: true ~loc name;
  generate_all_names ~quot: false ~loc name cds;
  generate_all_names ~quot: true ~loc name cds;
]

let generate ~loc ~path: _ (rec_flag, tds) =
  (
    match rec_flag with
    | Nonrecursive -> Location.raise_errorf ~loc "ppx_madge_wrapped_endpoints: nonrec not supported"
    | _ -> ()
  );
  match tds with
  | [td] ->
    (
      match td.ptype_kind with
      | Ptype_variant cds -> generate ~loc td.ptype_name.txt cds
      | _ -> Location.raise_errorf ~loc "ppx_madge_wrapped_endpoints: kind not supported"
    )
  | _ -> Location.raise_errorf ~loc "ppx_madge_wrapped_endpoints: no or multiple declarations not supported"

let _ =
  Deriving.add
    "madge_wrapped_endpoints"
    ~str_type_decl: (Deriving.Generator.make_noarg generate)
