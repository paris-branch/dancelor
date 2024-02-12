{
  perSystem = { pkgs, ... }: {
    packages.default = pkgs.ocamlPackages.buildDunePackage {
      pname = "dancelor";
      version = "dev";
      src = ../.;

      nativeBuildInputs = (with pkgs.ocamlPackages; [ menhir js_of_ocaml ])
        ++ (with pkgs; [ sassc ]);

      buildInputs = with pkgs.ocamlPackages; [
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
        ppx_monad
        slug
        yaml

        # documentation and tests
        odoc
      ];
    };
  };
}
