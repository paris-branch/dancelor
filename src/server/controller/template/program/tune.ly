\score {
  \header {
    piece = "%s"
    composer = "%s"
  }

  {
    \tocTune \markup { %s }

    \transpose %s c {
      %s
    }
  }
}
