{
  pkgs,
  tuneType,
  makeTuneSnippets,
  withArgumentType,
  ...
}:

let
  inherit (pkgs) lib;

  inherit (builtins)
    replaceStrings
    toJSON
    ;

  inherit (lib)
    concatMapStringsSep
    escapeShellArg
    mkOption
    types
    ;

  inherit (pkgs)
    runCommand
    ;

  forConcat = xs: f: concatMapStringsSep "\n" f xs;
  escapeLatexString = replaceStrings [ "&" "#" ] [ "\\&" "\\#" ];

  partType = types.submodule {
    options = {
      name = mkOption {
        description = "The name of the part.";
        type = types.str;
      };
    };
  };

  setType = types.submodule {
    options = {
      slug = mkOption {
        description = "A slug for the set; it is used to make logs clearer.";
        type = types.str;
      };
      name = mkOption {
        description = "The name of the set.";
        type = types.str;
      };
      conceptor = mkOption {
        description = "The “conceptor” line for the set.";
        type = types.str;
      };
      kind = mkOption {
        description = "The “kind” line for the set.";
        type = types.str;
      };
      contents = mkOption {
        description = "The contents of the set.";
        type = types.listOf tuneType;
      };
    };
  };

  bookType = types.submodule {
    options = {
      slug = mkOption {
        description = "A slug for the book; it is used to make logs clearer.";
        type = types.str;
      };
      title = mkOption {
        description = "The title of the book.";
        type = types.str;
      };
      editor = mkOption {
        description = "The editor of the book.";
        type = types.str;
      };
      contents = mkOption {
        description = "The contents of the book.";
        type = types.listOf (
          ## NOTE: This is a technique for the sum of submodule types. It is
          ## very complicated to do cleanly in practice, so we settle for
          ## something that allows a few more values than expected.
          types.submodule {
            options = {
              part = mkOption {
                type = types.nullOr partType;
                default = null;
              };
              set = mkOption {
                type = types.nullOr setType;
                default = null;
              };
            };
          }
        );
      };
      simple = mkOption {
        description = ''
          Whether the book should be simple, that is with no title page, no
          table of contents, and not two-sided.
        '';
        type = types.bool;
      };
    };
  };

  bookPdfArgType = types.submodule {
    options = {
      book = mkOption { type = bookType; };
      specificity = mkOption {
        description = "Specificity of this particular book, eg. Bb instruments or bass clef.";
        type = types.str;
      };
      headers = mkOption {
        description = "Whether the book should contain headers and footers.";
        type = types.bool;
      };
      pdf_metadata = mkOption {
        description = "PDF metadata";
        type = types.submodule {
          options = {
            title = mkOption { type = types.str; };
            authors = mkOption { type = types.listOf types.str; };
            subjects = mkOption { type = types.listOf types.str; };
          };
        };
      };
    };
  };

  makeBookPdf = withArgumentType "makeBookPdf" bookPdfArgType (
    {
      book,
      specificity,
      headers,
      pdf_metadata,
    }:
    runCommand "book-${book.slug}"
      {
        allowSubtitutes = false;
        buildInputs = [
          (pkgs.texlive.combine {
            inherit (pkgs.texlive)
              scheme-minimal
              latexmk
              xetex
              etoolbox
              extsizes
              fancyhdr
              fontspec
              geometry
              graphics
              greek-fontenc # dependency of hyperref
              hyperref
              realscripts # for \newif
              xltxtra
              xunicode
              ;
          })
        ];
        FONTCONFIG_FILE =
          with pkgs;
          makeFontsConf {
            fontDirectories = [ source-sans-pro ];
          };
      }
      ''
        cp ${./book}/*.tex .
        {
          printf '\\newif\\ifsimple\n'
          printf '\\simple${if book.simple then "true" else "false"}\n'
          printf '\\newif\\ifheaders\n'
          printf '\\headers${if headers then "true" else "false"}\n'
          printf '\\input{preamble}\n'
          printf '\\usepackage[\n'
          printf '  pdftitle={%s},\n' ${escapeShellArg (escapeLatexString pdf_metadata.title)}
          printf '  pdfauthor={%s},\n' ${
            escapeShellArg (concatMapStringsSep "; " escapeLatexString pdf_metadata.authors)
          }
          printf '  pdfsubject={%s},\n' ${
            escapeShellArg (concatMapStringsSep "; " escapeLatexString pdf_metadata.subjects)
          }
          printf '  pdfcreator={Dancelor},\n'
          printf ']{hyperref}\n'
          printf '\\begin{document}\n'
          printf '\\title{%s}\n' ${escapeShellArg (escapeLatexString book.title)}
          printf '\\author{%s}\n' ${escapeShellArg (escapeLatexString book.editor)}
          printf '\\specificity{%s}\n' ${escapeShellArg (escapeLatexString specificity)}
          ${
            if !book.simple then
              ''
                printf '\\maketitle\n\\break\n'
              ''
            else
              ""
          }
          ${forConcat book.contents (
            page:
            if page.part != null then
              ''
                printf '\\part{%s}\n' ${escapeShellArg (escapeLatexString page.part.name)}
              ''
            else if page.set != null then
              ''
                printf '\\begin{set}{%s}{%s}{%s}\n' \
                  ${escapeShellArg (escapeLatexString page.set.name)} \
                  ${escapeShellArg (escapeLatexString page.set.conceptor)} \
                  ${escapeShellArg (escapeLatexString page.set.kind)}

                ${forConcat page.set.contents (tune: ''
                  printf '\\tune{%s}{%s}{%s}{%s}\n' \
                    ${escapeShellArg (escapeLatexString tune.name)} \
                    ${escapeShellArg (escapeLatexString tune.instructions)} \
                    ${escapeShellArg (escapeLatexString tune.composer)} \
                    ${makeTuneSnippets tune}/snippet.pdf
                '')}

                printf '\\end{set}\n'
              ''
            else
              throw "Unexpected page type: ${toJSON page}"
          )}
          ${
            if !book.simple then
              ''
                printf '\\tableofcontents\n'
              ''
            else
              ""
          }
          printf '\\end{document}\n'
        } > book.tex
        # latexmk -f -interaction=nonstopmode -pdfxe book
        latexmk -pdfxe book
        mkdir $out
        mv book.pdf $out
      ''
  );

in
{
  inherit
    makeBookPdf
    ;
}
