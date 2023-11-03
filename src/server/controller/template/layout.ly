\layout {
  indent = 0

  \context {
    \Score

    %% Prevent LilyPond from breaking line inside a score without explicit
    %% notice.
    \override NonMusicalPaperColumn.line-break-permission = ##f

    %% Prevent LilyPond from breaking page inside a score without explicit
    %% notice.
    \override NonMusicalPaperColumn.page-break-permission = ##f
  }

  \context {
    \Staff

    %% Note head size relatively to the global staff size
    fontSize = #-0.4
  }

  \context {
    \ChordNames

    %% Chord name size relatively to the global staff size (?)
    \override ChordName.font-size = #0.3
  }
}
