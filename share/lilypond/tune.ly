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
    %#end-of-line-invisible
    %\override Score.barNumberVisibility = #(every-nth-bar-number-visible 8)
    % \remove "Bar_number_engraver"
  }
}

\score {
  {{{version.content}}}
}
