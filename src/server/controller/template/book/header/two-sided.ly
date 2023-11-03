\paper {
  oddHeaderMarkup = \markup {
    \unless \on-first-page \fill-line {
      \bold \fromproperty #'header:booktitle
      ""
    }
  }
  evenHeaderMarkup = \markup {
    \unless \on-first-page \fill-line {
      ""
      \fromproperty #'header:title
    }
  }
}
