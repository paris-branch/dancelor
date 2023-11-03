\paper {
  oddHeaderMarkup = \markup {
    \unless \on-first-page \fill-line {
      \bold \fromproperty #'header:booktitle
      \fromproperty #'header:title
    }
  }
  evenHeaderMarkup = \oddHeaderMarkup
}
