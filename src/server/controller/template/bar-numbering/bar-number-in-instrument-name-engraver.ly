#(define (put-bar-number-in-instrument-name! context)
  (let* ((barnum      (ly:context-property context 'currentBarNumber))
         (moment-zero (ly:make-moment 0))
         (measure-pos (ly:context-property context 'measurePosition moment-zero))
         (formatter   (ly:context-property context 'barNumberFormatter))
         (markup      (formatter barnum measure-pos -1 context)))
   (ly:context-set-property! context 'instrumentName markup)
   (ly:context-set-property! context 'shortInstrumentName markup)))

#(define (Bar_number_in_instrument_name_engraver context)
  (make-engraver
   (listeners
    (
      (note-event engraver event)
      (put-bar-number-in-instrument-name! context)
    )
    (
      (rest-event engraver event)
      (put-bar-number-in-instrument-name! context)
    )
  )))

#(ly:register-translator
  Bar_number_in_instrument_name_engraver 'Bar_number_in_instrument_name_engraver
  '((grobs-created . ())
    (events-accepted . ())
    (properties-read . (currentBarNumber measurePosition barNumberFormatter))
    (properties-written . (instrumentName shortInstrumentName))
    (description . "Places the of bar number in the instrument name properties")))
