\layout {
  indent = 0

  \context {
    \Score

    %% Prevent Lilypond from breaking inside a score without explicit notice.
    \override NonMusicalPaperColumn.page-break-permission = ##f

    %% Show bar numbers at the beginning and middle of lines; but not the end.
    \override BarNumber.break-visibility = ##(#f #t #t)

    %% Write the number at the beginning of the bar it's denoting (and not the
    %% end of the previous one).
    \override BarNumber.self-alignment-X = #LEFT

    %% Show number of bars equal to 1 modulo 8 (1, 9, 17, etc.)
    barNumberVisibility = #(modulo-bar-number-visible 8 1)

    %% Count alternatives in numbering.
    alternativeNumberingStyle = #'numbers
  }
}

#(set-global-staff-size 18)
