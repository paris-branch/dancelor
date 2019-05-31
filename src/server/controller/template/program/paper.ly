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

  %%two-sided = ##t
  %%inner-margin = 20\mm
  %%outer-margin = 10\mm

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
    \on-the-fly \not-first-page \fill-line {
      \bold \fromproperty #'header:booktitle
      \fromproperty #'header:title
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