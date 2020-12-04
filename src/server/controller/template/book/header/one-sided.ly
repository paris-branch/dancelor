\paper {
  oddHeaderMarkup = \markup {
    \on-the-fly \not-first-page \fill-line {
      \bold \fromproperty #'header:booktitle
      \fromproperty #'header:title
    }
  }
  evenHeaderMarkup = \oddHeaderMarkup
}
