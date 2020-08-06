\layout {
  \context {
    \Score

    %% Show bar numbers at the beginning of lines only
    \override BarNumber.break-visibility = ##(#f #f #t)

    %% Show all bar numbers, including the first one and including broken bars.
    %% This is all then subject to `BarNumber.break-visibility`.
    barNumberVisibility = #all-bar-numbers-visible

    %% Write the number at the beginning of the bar it's denoting (and not the
    %% end of the previous one).
    \override BarNumber.self-alignment-X = #LEFT
  }
}
