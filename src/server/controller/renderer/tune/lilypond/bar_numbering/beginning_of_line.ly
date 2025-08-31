\layout {
  \context {
    \Score
    \remove "Bar_number_engraver"
  }

  \context {
    \Staff
    barNumberFormatter = #bar-numbers-formatter
    \consists "Bar_number_in_instrument_name_engraver"
    \override InstrumentName.font-size = #-2
    \override InstrumentName.padding = #1
  }
}
