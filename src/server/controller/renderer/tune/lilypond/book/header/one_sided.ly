\paper {
  oddHeaderMarkup = \markup {
    \fill-line {
      \fromproperty #'header:booktitle
      \on-the-fly \not-part-first-page {
        \fromproperty #'header:title
      }
    }
  }
  evenHeaderMarkup = \oddHeaderMarkup
}
