\paper {
  evenHeaderMarkup = \markup {
    \on-the-fly \not-first-page \fill-line {
      \fromproperty #'header:booktitle
      ""
    }
  }
  oddHeaderMarkup = \markup {
    \on-the-fly \not-first-page \fill-line {
      ""
      \fromproperty #'header:title
    }
  }
}
