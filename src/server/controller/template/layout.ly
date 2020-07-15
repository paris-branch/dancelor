\defineBarLine "[|:-||" #'("||" "[|:" "")

%% Define an other barNumberFormatter. This one prints broken bars as if they
%% were already in the next bar. It only makes sense at the beginning of a line.
#(define-public (partial-aware-bar-number-formatter barnum measure-pos alt-number context)
  (let* ((begin-measure (= 0 (ly:moment-main-numerator measure-pos)))
         (broken-bar (and (not begin-measure) (!= barnum 1)))
         (barnum (+ barnum (if broken-bar 1 0))))
   (markup (number->string barnum))))

\layout {
  indent = 0

  \context {
    \Score

    %% Prevent Lilypond from breaking inside a score without explicit notice.
    \override NonMusicalPaperColumn.page-break-permission = ##f

    %% Show all bar numbers, including the first one and including broken bars.
    %% This is all then subject to `BarNumber.break-visibility`.
    barNumberVisibility = #all-bar-numbers-visible

    %% Show bar numbers at the beginning of lines only
    \override BarNumber.break-visibility = ##(#f #f #t)

    %% Use the previously-defined "partial-aware" formatter.
    barNumberFormatter = #partial-aware-bar-number-formatter

    %% Write the number at the beginning of the bar it's denoting (and not the
    %% end of the previous one).
    \override BarNumber.self-alignment-X = #LEFT

    %% Count alternatives in numbering.
    alternativeNumberingStyle = #'numbers

    %% Bigger repeat signs
    startRepeatType = #"[|:-||"
    endRepeatType = #":|]"
    doubleRepeatType = #":|][|:"
  }

  \context {
    \Staff

    %% Note head size relatively to the global staff size
    fontSize = #-0.4
  }

  \context {
    \ChordNames

    %% Chord name size relatively to the global staff size (?)
    \override ChordName #'font-size = #0.3
  }
}

#(set-global-staff-size 18)
