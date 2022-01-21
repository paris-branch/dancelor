\score {
  \header {
    piece = "%s"
    opus = "%s"
  }

  \layout {}

  \transpose %s %s {
    %s
  }
}

%%%% FIXME
#(set! make-music the-make-music)

\score {
  \midi { \tempo %s = %d }
  \%sChords \fancyUnfoldRepeats {
    \transpose %s %s {
       %s
    }
  }
}
