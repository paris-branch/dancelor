\score {
  \header {
    piece = "%s"
    opus = "%s"
  }

  \layout {}

  %s
}

%%%% FIXME
#(set! make-music the-make-music)

\score {
  \midi { \tempo %s = %d }
  \fancyUnfoldRepeats %s
}
