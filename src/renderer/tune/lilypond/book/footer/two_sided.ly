\paper {
  oddFooterMarkup = \markup {
    \on-the-fly \not-first-page \fill-line {
      \concat { \fromproperty #'header:instrument }
      \concat { \fromproperty #'page:page-number-string }
    }
  }
  evenFooterMarkup = \markup {
    \on-the-fly \not-first-page \fill-line {
      \concat { \fromproperty #'page:page-number-string }
      \concat { \fromproperty #'header:instrument }
    }
  }
}
