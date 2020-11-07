\score {
  \header {
    piece = "%s"
    opus = "%s"
  }

  \layout {
    \context {
      \Score
      currentBarNumber = #%d
    }
  }

  \transpose %s %s {
    %s
  }
}
