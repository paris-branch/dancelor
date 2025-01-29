\paper {
  oddFooterMarkup = \markup {
    \fill-line {
      \concat { \fromproperty #'header:instrument }
      \concat { \fromproperty #'page:page-number-string }
    }
  }
  evenFooterMarkup = \markup {
    \fill-line {
      \concat { \fromproperty #'page:page-number-string }
      \concat { \fromproperty #'header:instrument }
    }
  }
}
