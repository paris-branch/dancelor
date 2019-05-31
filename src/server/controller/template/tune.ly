\version "2.19.82"

\header {
  tagline = ""
}

\paper {
  indent = 0
}

\layout {
  \context {
    \Score
    \override Score.BarNumber.break-visibility = ##(#t #t #t)
    \remove "Bar_number_engraver"
  }
}

\score {
  %s
}
