(** {1 Madge's endpoints wrapper}

    Madge features GADT-style endpoints that carry extra type information. This
    makes it hard to manipulate lists of endpoints. However, it is frequent to
    want to iterate over all endpoints to find one that might be matching, for
    instance. The solution for this purpose is to use another GADT to hide the
    type information. Typically, one would do:

    {|
      type wrapped = W : ('a, 'w, 'r) endpoint -> wrapped
    |}

    or, in fact, more useful:

    {|
      type wrapped = W : ('a, 'r Lwt.t, 'r) endpoint -> wrapped
    |}

    One can then put values of type [wrapped] in the same list without issue.
    When matching on [W] and getting the value it wraps, the type equalities
    will still be there, happily waiting, and we will be able to use the
    endpoint in question.

    This PPX automatises the definition of this type, and, more importantly, the
    definition of a value, [all], containing all the endpoints of the given
    type, wrapped. The usage is as follows:

    {|
      type ('a, 'w, 'r) sub_endpoint =
        | Foo : ((string -> 'w), 'w, int) sub_endpoint
      [@@deriving madge_wrapped_endpoints]

      type ('a, 'w, 'r) t =
        | Bar : ((float -> 'w), 'w, bool) t
        | Lift : ('a, 'w, 'r) sub_endpont -> ('a, 'w, 'r) t
      [@@deriving madge_wrapped_endpoints]
    |}

    This will generate, among other things:

    {|
      type wrapped_sub_endpoint = W_sub_endpoint : ('a, 'r Lwt.t, 'r) sub_endpoint -> wrapped_sub_endpoint
      let all_sub_endpoints : wrapped_sub_endpoint list = [W_sub_endpoint Foo]

      type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
      let all : wrapped list = List.flatten [
        [W Bar];
        List.map (fun (W_sub_endpoint e) -> W (Lift e)) all_sub_endpoints;
      ]
    |}

    allowing to iterate on [all] endpoints easily.

    Sometimes, however, one wants to match on all endpoints, but not with the
    goal of applying the corresponding constructors. For this reason, this PPX
    also produces:

    {|
      type 'w wrapped_endpoint' =
        W_endpoint' : ('a, 'w, 'r) endpoint -> 'w wrapped_endpoint'

      val all_endpoints' : unit -> 'w wrapped_endpoint' list
    |}

    In such a wrapping, the return type is lost, but it is possible to inject
    any type of one's choosing. The [unit ->] type is to avoid type issues where
    ['w] cannot be generalised. *)

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

let map_lident_last f = function
  | Lident x -> Lident (f x)
  | Ldot (li, x) -> Ldot (li, f x)
  | _ -> assert false

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
    {|
      type wrapped_<name> = W_<name> : ('a, 'r Lwt.t, 'r) <name> -> wrapped_<name>
    |}
    or
    {|
      type 'w wrapped_<name>' = W_<name>' : ('a, 'w, 'r) <name> -> 'w wrapped_<name>'
    |}
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
    {|
      let all_<name>s : wrapped list =
        List.flatten
          [
            [W_<name> <Cstr1>];
            List.map (fun (W_<name2> endpoint) -> W_<name> (<Cstr2> endpoint)) all_<name2>s;
          ]
    |}
    or
    {|
      let all_<name>s' : unit -> 'w wrapped' list =
        fun () ->
          List.flatten
            [
              [W_<name>' <cstr1>];
              List.map (fun (W_<name2>' endpoint) -> W_<name>' (<Cstr2> endpoint)) all_<name2>s';
            ]
    |}
    depending on the [~quot] argument. <name2> is the name of the type that
    <Cstr2> lifts. *)
let generate_all_names ~quot ~loc name cds =
  pstr_value ~loc Nonrecursive [
    value_binding
      ~loc
      ~pat: (
        (* all_<name>s : wrapped list *)
        (* all_<name>s' : unit -> 'w wrapped' list *)
        ppat_constraint
          ~loc
          (ppat_var ~loc @@ Loc.make ~loc @@ all_names ~quot name)
          (
            let wrapped_list =
              ptyp_constr ~loc (loc_lident ~loc "list") [
                ptyp_constr
                  ~loc
                  (loc_lident ~loc @@ wrapped_name ~quot name)
                  (if quot then [ptyp_var ~loc "w"] else []);
              ]
            in
            if quot then
              ptyp_arrow ~loc Nolabel (ptyp_constr ~loc (loc_lident ~loc "unit") []) wrapped_list
            else
              wrapped_list
          )
      )
      ~expr: (
        (* List.flatten ... *)
        (* fun () -> List.flatten ... *)
        let list_flatten_etc =
          pexp_apply
            ~loc
            (pexp_ident ~loc @@ loc_lident ~loc "List.flatten")
            [
              Nolabel,
              (* [ ... ] *)
              pexp_list ~loc (
                List.map
                  (fun cd ->
                    match cd.pcd_args with
                    | Pcstr_tuple [] ->
                      (* [W_<name> <Cstr1>] *)
                      (* [W_<name>' <Cstr1>] *)
                      pexp_list ~loc [
                        pexp_construct
                          ~loc
                          (loc_lident ~loc @@ w_name ~quot name)
                          (Some (pexp_construct ~loc (loc_lident ~loc cd.pcd_name.txt) None))
                      ]
                    | Pcstr_tuple [{ptyp_desc = Ptyp_constr ({txt = long_name2; _}, _); _}] ->
                      (* List.map (fun (W_<name2> endpoint) -> W_<name> (<Cstr2> endpoint)) all_<name2>s; *)
                      (* List.map (fun (W_<name2> endpoint) -> W_<name>' (<Cstr2> endpoint)) all_<name2>s; *)
                      pexp_apply
                        ~loc
                        (pexp_ident ~loc @@ loc_lident ~loc "List.map")
                        [
                          (
                            Nolabel,
                            (* fun (W_<name2> endpoint) -> W_<name> (<Cstr2> endpoint) *)
                            (* fun (W_<name2>' endpoint) -> W_<name>' (<Cstr2> endpoint) *)
                            pexp_fun
                              ~loc
                              Nolabel
                              None
                              (
                                (* (W_<name2> endpoint) *)
                                ppat_construct
                                  ~loc
                                  (Loc.make ~loc @@ map_lident_last (w_name ~quot) long_name2)
                                  (Option.some @@ ppat_var ~loc @@ Loc.make ~loc "endpoint")
                              )
                              (
                                (* W_<name> (<Cstr2> endpoint) *)
                                (* W_<name>' (<Cstr2> endpoint) *)
                                pexp_construct
                                  ~loc
                                  (loc_lident ~loc @@ w_name ~quot name)
                                  (
                                    Option.some @@
                                      pexp_construct
                                        ~loc
                                        (loc_lident ~loc cd.pcd_name.txt)
                                        (Option.some @@ pexp_ident ~loc @@ loc_lident ~loc "endpoint")
                                  )
                              )
                          );
                          (
                            Nolabel,
                            (* all_<name2>s *)
                            (* all_<name2>s' () *)
                            let all_names2 =
                              pexp_ident ~loc @@ Loc.make ~loc @@ map_lident_last (all_names ~quot) long_name2
                            in
                            if quot then pexp_apply ~loc all_names2 [Nolabel, pexp_construct ~loc (loc_lident ~loc "()") None]
                            else all_names2
                          );
                        ]
                    | _ ->
                      Location.raise_errorf
                        ~loc: cd.pcd_loc
                        "ppx_madge_wrapped_endpoints: constructor with arguments not supported"
                  )
                  cds
              )
            ]
        in
        if quot then
          pexp_fun
            ~loc
            Nolabel
            None
            (ppat_construct ~loc (loc_lident ~loc "()") None)
            list_flatten_etc
        else
          list_flatten_etc
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

let _ = Deriving.add "madge_wrapped_endpoints" ~str_type_decl: (Deriving.Generator.make_noarg generate)
