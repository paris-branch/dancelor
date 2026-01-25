\version "2.18.2"

\layout {
  indent = 0
  \context { \Score
    %% Prevent LilyPond from breaking line inside a score without explicit
    %% notice.
    \override NonMusicalPaperColumn.line-break-permission = ##f
    %% Prevent LilyPond from breaking page inside a score without explicit
    %% notice.
    \override NonMusicalPaperColumn.page-break-permission = ##f
  }
  \context { \Staff
    %% Note head size relatively to the global staff size
    fontSize = #-0.4
  }
  \context { \ChordNames
    %% Chord name size relatively to the global staff size (?)
    \override ChordName.font-size = #0.3
  }
}

\paper {
  indent = 0
  property-defaults.fonts.roman = "Source Sans 3"
  property-defaults.fonts.sans = "Source Sans 3"
  property-defaults.fonts.typewriter = "Source Sans 3"
  ragged-right = ##f
  ragged-bottom = ##t
  ragged-last-bottom = ##t
  markup-markup-spacing = #'((basic-distance . 15) (padding . 0.5))
  score-markup-spacing = #'((basic-distance . 15) (padding . 0.5) (stretchability . 60))
}

%% Paper specific to cropping.
\paper {
  %% Make the page as big as its content
  page-breaking = #ly:one-page-breaking
  %% Remove horizontal margins; keep 1cm on the left for bar numbers
  two-sided = #f
  left-margin = 0
  right-margin = 1\cm
  %% Remove all vertical margins
  top-margin = 0
  bottom-margin = 0
  %% Remove most other flexible vertical spacing
  score-system-spacing = 0
  markup-system-spacing = 0
  score-markup-spacing = 0
  markup-markup-spacing = 0
  top-system-spacing = 0
  top-markup-spacing = 0
  last-bottom-spacing = 0
}

%% Define fancy volta repeat brackets, and use them by default.
\defineBarLine "[|:-||" #'("||" "[|:" "")
\layout {
  \context { \Score
    startRepeatBarType = #"[|:-||"
    endRepeatBarType = #":|]"
    doubleRepeatBarType = #":|][|:"
  }
}

%% Always remove the tagline.
\header {
  tagline = ""
}
