\paper {
  oddHeaderMarkup = \markup {
    \on-the-fly \not-first-page \fill-line {
      \fromproperty #'header:booktitle
      ""
    }
  }
  evenHeaderMarkup = \markup {
    \on-the-fly \not-first-page \fill-line {
      ""
      \fromproperty #'header:title
    }
  }
}
