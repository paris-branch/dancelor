\score {
  \header {
    piece = "%s"
    composer = "%s"
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
