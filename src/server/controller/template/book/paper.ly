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
  tocVersionMarkup = \markup
  \fill-line {
    \concat {
      \hspace #5
      \italic
      \fromproperty #'toc:text
    }
    \null
  }

  left-margin = 10\mm
  right-margin = 10\mm

  bookTitleMarkup = \markup {
    \fill-line {
      \dir-column {
        \center-align \bold \abs-fontsize #26 \fromproperty #'header:title
        \center-align \large \fromproperty #'header:deviser
        \center-align \large \fromproperty #'header:kind
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

  oddFooterMarkup = \markup {
    \unless \on-first-page \fill-line {
      \concat { \fromproperty #'header:instrument }
      \concat { \fromproperty #'page:page-number-string }
    }
  }
  evenFooterMarkup = \markup {
    \unless \on-first-page \fill-line {
      \concat { \fromproperty #'page:page-number-string }
      \concat { \fromproperty #'header:instrument }
    }
  }
}
