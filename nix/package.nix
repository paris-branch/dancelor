{
  perSystem =
    { self', pkgs, ... }:
    {
      packages.default = pkgs.ocamlPackages.buildDunePackage {
        pname = "dancelor";
        version = "dev";
        src = ../.;

        nativeBuildInputs =
          (with pkgs.ocamlPackages; [
            menhir
            js_of_ocaml
          ])
          ++ (with pkgs; [ sassc ]);

        buildInputs = with pkgs.ocamlPackages; [
          self'.packages.ocaml-argon2
          self'.packages.ppx_yojson_conv

          cohttp-lwt-jsoo
          cohttp-lwt-unix
          dates_calc
          iso8601
          js_of_ocaml-ppx
          js_of_ocaml-tyxml
          logs
          lwt_ppx
          lwt_react
          ppx_blob
          ppx_import
          ppx_inline_test
          ppx_fields_conv
          ppx_monad
          slug
          yaml

          # documentation and tests
          alcotest
          odoc
          ppx_deriving_qcheck
          qcheck
          qcheck-alcotest

          # used for vendored `ppx_variants_conv`
          variantslib
        ];
      };

      packages.ocaml-argon2 = pkgs.ocamlPackages.callPackage ./package/ocaml/argon2.nix { };
      packages.ppx_yojson_conv = pkgs.ocamlPackages.callPackage ./package/ocaml/ppx_yojson_conv.nix { };
    };
}
