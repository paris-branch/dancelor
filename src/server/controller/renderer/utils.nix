{ ... }:

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

  ## Setup script to configure fontconfig with a writable cache directory. This
  ## prevents "No writable cache directories" warnings from fontconfig.
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

in
{
  inherit
    withArgumentType
    setupFontconfigCache
    ;
}
