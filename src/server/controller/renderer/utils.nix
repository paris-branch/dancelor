{ pkgs, ... }:

let
  withArgumentType =
    name: type: cont: args:
    if !(type.check args) then
      throw "The value passed to `${name}` does not have the expected type."
    else
      let
        merged =
          type.merge
            [ ]
            [
              {
                value = args;
                file = "argument passed to ${name}";
              }
            ];
      in
      cont merged;

  ## Setup script to configure fontconfig and luaotfload with a writable cache directories.
  ## This prevents "No writable cache directories" warnings from fontconfig and avoids
  setupFontconfigCache = ''
    export HOME=$(mktemp -d)
    mkdir -p "$HOME"/.cache/fontconfig
    cat <<EOF >$HOME/fonts.conf
    <?xml version="1.0"?>
    <fontconfig>
      <include>$FONTCONFIG_FILE</include>
      <cachedir>$HOME/.cache/fontconfig</cachedir>
    </fontconfig>
    EOF
    export FONTCONFIG_FILE=$HOME/fonts.conf
  '';

  myTexlive = pkgs.texliveFull;
  ## FIXME: minimise, but the old minimisation doesn't work for lualatex anymore:
  # (pkgs.texlive.combine {
  #   inherit (pkgs.texlive)
  #     scheme-minimal
  #     latexmk
  #     luatex
  #     xetex
  #     etoolbox
  #     extsizes
  #     fancyhdr
  #     fontspec
  #     geometry
  #     graphics
  #     greek-fontenc # dependency of hyperref
  #     hyperref
  #     realscripts # for \newif
  #     texfot
  #     xltxtra
  #     xunicode
  #     ;
  # })

  myFontconfigFile =
    with pkgs;
    makeFontsConf {
      fontDirectories = [ source-sans-pro ];
    };

  ## The luaotfload cache is computed when starting LuaLaTeX if not
  ## precomputed, and that takes a few seconds. We save them by
  ## storing it in the Nix store permanently and pointing subsequent
  ## runs of LuaLaTeX to it.
  ##
  luaotfloadCache =
    pkgs.runCommand "luaotfload-cache"
      {
        preferLocalBuild = true;
        allowSubstitutes = false;
        buildInputs = [ myTexlive ];
        FONTCONFIG_FILE = myFontconfigFile;
      }
      ''
        ${setupFontconfigCache}
        mkdir -p $out
        TEXMFCACHE=$out luaotfload-tool --update --force
      '';

  setupLuaotfloadCache = ''
    mkdir -p texmf-cache
    cp -r ${luaotfloadCache}/* texmf-cache/
    chmod -R u+w texmf-cache/
    export TEXMFVAR=$PWD/texmf-cache
  '';

in
{
  inherit
    withArgumentType
    setupFontconfigCache
    setupLuaotfloadCache
    myTexlive
    myFontconfigFile
    ;
}
