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

%% The following is important for the cropped SVG output; otherwise the last
%% line of chords causes troubles.
\markup\null
