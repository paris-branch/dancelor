{
  perSystem =
    {
      self',
      inputs',
      pkgs,
      lib,
      ...
    }:
    {
      packages.dancelor = pkgs.ocamlPackages.buildDunePackage {
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
          self'.packages.prometheus-app
          inputs'.camelotte.packages.monadise
          inputs'.camelotte.packages.monadise-lwt

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
          ppx_deriving_yojson
          ppx_import
          ppx_inline_test
          ppx_fields_conv
          ppx_monad
          ppx_variants_conv
          prometheus
          slug
          yaml
        ];

        ## TODO: The tests run `qcheck`; it would be good if we had a way to run
        ## them with the additional environment variable QCHECK_LONG=true.
        doCheck = true;
        checkInputs = with pkgs.ocamlPackages; [
          alcotest
          ppx_deriving_qcheck
          qcheck
          qcheck-alcotest
        ];
      };

      packages.documentation =
        let
          super = self'.packages.dancelor;
        in
        pkgs.stdenv.mkDerivation {
          name = "${super.name}-documentation";
          ## Grabbing super's buildInputs is overkill in terms of dependencies,
          ## but most often we will also build the package, so it is fine.
          inherit (super) src nativeBuildInputs;
          buildInputs = super.buildInputs ++ [ pkgs.ocamlPackages.odoc ];
          buildPhase = "dune build @doc";
          installPhase = "cp -R _build/default/_doc/_html $out";
        };

      packages.ocaml-argon2 = pkgs.ocamlPackages.buildDunePackage {
        pname = "argon2";
        version = "dev";
        src = pkgs.fetchFromGitHub {
          owner = "khady";
          repo = "ocaml-argon2";
          rev = "1.0.2";
          sha256 = "sha256-m5yOMT33Z9LfjQg6QRBW6mjHNyIySq6somTFuGmL9xI=";
        };

        propagatedBuildInputs =
          (with pkgs; [ libargon2 ])
          ++ (with pkgs.ocamlPackages; [
            ctypes-foreign
          ]);

        buildInputs = with pkgs.ocamlPackages; [
          ctypes
          dune-configurator
          result
        ];
      };

      packages.prometheus-app = pkgs.ocamlPackages.buildDunePackage rec {
        pname = "prometheus-app";
        version = "1.2";

        src = pkgs.fetchurl {
          url = "https://github.com/mirage/prometheus/releases/download/v${version}/prometheus-${version}.tbz";
          sha256 = "sha256-g2Q6ApprbecdFANO7i6U/v8dCHVcSkHVg9wVMKtVW8s=";
        };

        duneVersion = "3";

        propagatedBuildInputs = with pkgs.ocamlPackages; [
          alcotest
          astring
          asetmap
          cohttp-lwt-unix
          fmt
          logs
          lwt
          prometheus
          re
        ];

        meta = {
          description = "Client library for Prometheus monitoring";
          license = lib.licenses.asl20;
          maintainers = [ lib.maintainers.ulrikstrid ];
        };
      };
    };
}
