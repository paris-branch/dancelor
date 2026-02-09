{
  pkgs,
  tuneType,
  makeTuneSnippets,
  withArgumentType,
  setupFontconfigCache,
  ...
}:

let
  inherit (pkgs)
    lib
    writeText
    ;

  inherit (builtins)
    replaceStrings
    toJSON
    ;

  inherit (lib)
    concatMapStringsSep
    mkOption
    types
    optionalString
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

  makeBookTex =
    {
      book,
      specificity,
      headers,
      pdf_metadata,
    }:
    writeText "book-${book.slug}.tex" ''
      \newif\ifsimple
      \simple${if book.simple then "true" else "false"}
      \newif\ifheaders
      \headers${if headers then "true" else "false"}
      \input{preamble}
      \usepackage[
        pdftitle={${escapeLatexString pdf_metadata.title}}
        pdfauthor={${concatMapStringsSep "; " escapeLatexString pdf_metadata.authors}}
        pdfsubject={${concatMapStringsSep "; " escapeLatexString pdf_metadata.subjects}}
        pdfcreator={Dancelor},
      ]{hyperref}
      \begin{document}
      \title{${escapeLatexString book.title}}
      \author{${escapeLatexString book.editor}}
      \specificity{${escapeLatexString specificity}}
      ${optionalString (!book.simple) "\\maketitle\\break\n"}
      ${forConcat book.contents (
        page:
        if page.part != null then
          ''
            \part{${escapeLatexString page.part.name}}
          ''
        else if page.set != null then
          ''
            \begin{set}{${escapeLatexString page.set.name}}{${escapeLatexString page.set.conceptor}}{${escapeLatexString page.set.kind}}
              ${forConcat page.set.contents (tune: ''
                \tune{${escapeLatexString tune.name}}{${escapeLatexString tune.instructions}}{${escapeLatexString tune.composer}}{${makeTuneSnippets tune}/snippet.pdf}
              '')}
            \end{set}
          ''
        else
          throw "Unexpected page type: ${toJSON page}"
      )}
      ${optionalString (!book.simple) "\\tableofcontents\n"}
      \end{document}
    '';

  makeBookPdf = withArgumentType "makeBookPdf" bookPdfArgType (
    book@{ ... }:
    runCommand "book-${book.book.slug}"
      {
        preferLocalBuild = true;
        allowSubstitutes = false;
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
              texfot
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
        ${setupFontconfigCache}
        cp ${./book}/*.tex .
        cp ${makeBookTex book} book.tex
        texfot latexmk -pdfxe book
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
