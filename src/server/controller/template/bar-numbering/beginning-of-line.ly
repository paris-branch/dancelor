\layout {
  \context {
    \Score

    %% Show bar numbers at the beginning of lines only
    \override BarNumber.break-visibility = ##(#f #f #t)

    %% Show all bar numbers, including the first one and including broken bars.
    %% This is all then subject to `BarNumber.break-visibility`.
    barNumberVisibility = #all-bar-numbers-visible

    %% Hack the position so that it is at the beginning of the staff, centered.
    %% This only works nicely if the bar number markup is Y centered. It also
    %% relies on the size of the staff and is thus not so reliable.
    \override BarNumber.break-align-symbols = #'(left-edge)
    \override BarNumber.X-offset = #-1.3
    \override BarNumber.Y-offset = #-3.7
  }
}
