\score {
  \header {
    piece = \markup { \combine \transparent Aj "%s" }
    composer = \markup { \combine \transparent Aj "%s" }
  }

  \layout {
    \context {
      \Score
      currentBarNumber = #%d
    }
  }

  {
    \tocVersion \markup { %s }

    \transpose %s %s {
      %s
    }
  }
}
