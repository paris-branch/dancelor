\paper {
  oddHeaderMarkup = \markup {
    \on-the-fly \not-first-page {
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
    \on-the-fly \not-first-page {
      \fill-line {
        " "
        \concat { "Page " \fromproperty #'page:page-number-string }
      }
    }
  }
  evenFooterMarkup = \oddFooterMarkup
}
