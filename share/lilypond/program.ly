\version "2.19.82"

tocSet = #(define-music-function (parser location text) (markup?)
           (add-toc-item! 'tocSetMarkup text))
tocTune = #(define-music-function (parser location text) (markup?)
            (add-toc-item! 'tocTuneMarkup text))

#(define-markup-command (my-wordwrap-string layout props align strg)
  (number? string?)
  #:properties ((baseline-skip))
  #:category align
  "Same as @code{wordwrap-field}, but internally a stencil-list is produced
  first, which will aligned according to @var{align}, putting out a single
  stencil."

  ;; c/p from define-markup-commands.scm, because it's not public
  (define (general-column align-dir baseline mols)
   "Stack @var{mols} vertically, aligned to  @var{align-dir} horizontally."
   (let* ((aligned-mols
    (map (lambda (x) (ly:stencil-aligned-to x X align-dir)) mols))
    (stacked-stencil (stack-lines -1 0.0 baseline aligned-mols))
    (stacked-extent (ly:stencil-extent stacked-stencil X)))
    (ly:stencil-translate-axis stacked-stencil (- (car stacked-extent)) X)))

  (general-column
    align
    baseline-skip
    (wordwrap-string-internal-markup-list layout props #f strg)))

\layout {
  indent = 0
  \context {
    \Score
    \override NonMusicalPaperColumn.page-break-permission = ##f
  }
}

booktitle = "{{{name}}}"

{{#transpose}}
instrument = "{{instrument}}"
{{/transpose}}{{^transpose}}
instrument = "C"
{{/transpose}}

\paper {
  %% There is already a bookpart for the table of contents, no need to
  %% have a title.
  tocTitleMarkup = \markup\null

  tocSetMarkup = \markup \huge {
    \vspace #2
    \fill-with-pattern #1.5 #CENTER .
    \fromproperty #'toc:text
    \fromproperty #'toc:page
  }
  tocTuneMarkup = \markup
  \fill-line {
    \concat {
      \hspace #5
      \italic
      \fromproperty #'toc:text
    }
    \null
  }

  ragged-right = ##f
  ragged-bottom = ##t
  ragged-last-bottom = ##t
  markup-markup-spacing = #'((basic-distance . 15) (padding . 0.5))
  score-markup-spacing = #'((basic-distance . 15) (padding . 0.5) (stretchability . 60))

  %two-sided = ##t
  %inner-margin = 20\mm
  %outer-margin = 10\mm

  bookTitleMarkup = \markup {
    \fill-line {
      \dir-column {
        \center-align \bold \abs-fontsize #26 \fromproperty #'header:title
        \center-align \large \concat {
          \fromproperty #'header:kind
        }
        " "
      }
    }
  }

  scoreTitleMarkup = \markup {
    \fill-line {
      \fromproperty #'header:piece
      \dir-column {
        \right-align \fromproperty #'header:composer
        \right-align {
          \concat {
            \fromproperty #'header:arranger
          }
        }
      }
    }
  }

  oddHeaderMarkup = \markup {
    \on-the-fly \not-first-page {
      \column {
        \fill-line {
          \bold \fromproperty #'header:booktitle
          \fromproperty #'header:title
        }
        " "
      }
    }
  }
  evenHeaderMarkup = \oddHeaderMarkup

  oddFooterMarkup = \markup {
    \on-the-fly \not-first-page \fill-line {
      \concat { \fromproperty #'header:instrument " instruments" }
      \concat { "Page " \fromproperty #'page:page-number-string }
    }
  }
  evenFooterMarkup = \oddFooterMarkup
}

\book {
  \header {
    booktitle = \booktitle
    instrument = \instrument
  }

  \markuplist {
    \fill-line {
      \column {
        \center-align {
          \vspace #12
          \bold \abs-fontsize #48 \my-wordwrap-string #CENTER \booktitle
          \vspace #10
          \abs-fontsize #16 \concat { \instrument " instruments" }
        }
      }
    }
  }
  \pageBreak

  {{#sets}}
  \bookpart {
    \paper {
      page-count = 2
    }

    \tocSet \markup { {{{name}}} ({{kind}}) }

    \header {
      title = "{{{name}}}"
      kind = "{{kind}}"
    }

    {{#tunes}}
    \score {
      \header {
        piece = "{{{group.name}}}"
        composer = "{{#group.author}}{{{line}}}{{/group.author}}"
      }

      {
        \tocTune \markup { {{{group.name}}} }

        {{#transpose}}\transpose {{{target}}} c { {{/transpose}}
        {{{content}}}
        {{#transpose}} } {{/transpose}}
      }
    }
    {{/tunes}}

    %% When we tell Lilypond that sets must take two pages, it
    %% systematically puts the last component of the bookpart on the
    %% second page. Hence this null markup in order to avoid one score
    %% leaving its friends.
    \markup\null
  }
  {{/sets}}

  \bookpart {
    \header {
      title = "Table of Contents"
    }

    \paper {
      page-breaking = #ly:minimal-breaking
    }

    \markuplist \table-of-contents
  }
}
