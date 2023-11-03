\paper {
  oddHeaderMarkup = \markup {
    \unless \on-first-page {
      \dir-column {
        \fill-line {
          \bold \fromproperty #'header:title
          \fromproperty #'header:instrument
        }

        " "
      }
    }
  }
  evenHeaderMarkup = \oddHeaderMarkup

  oddFooterMarkup = \markup {
    \unless \on-first-page {
      \fill-line {
        " "
        \concat { "Page " \fromproperty #'page:page-number-string }
      }
    }
  }
  evenFooterMarkup = \oddFooterMarkup
}
