
#(define-markup-command (apply-offsets-list layout props barnum offsets) (number? list?)
  (let ((barnum (fold (lambda (offset barnum) (+ barnum (unbox offset))) barnum offsets)))
   (interpret-markup layout props
    (markup (number->string barnum)))))

#(define-markup-command (bar-numbers layout props barnum offsets-lists) (number? list?)
  (let ((barnums (map (lambda (offsets) #{ \markup \apply-offsets-list #barnum #offsets #}) offsets-lists)))
   (interpret-markup layout props
    #{ \markup \override #'(baseline-skip . 2) \right-column #barnums #})))

%% This file provides two ways of printing the several measure numbers: either
%% via a bar number formatter that can be used in the 'barNumberFormatter'
%% property of the context 'Score', or via an engraver that can be used to fill
%% the 'instrumentName' property with the bar numbers, thus achieving
%% beginning-of-line bar numbers.

#(define (bar-numbers-formatter barnum measure-pos alt-number context)
  (let ((barnum (partial-aware-bar-number context))
        (offsets-lists (list-repeat-offsets-lists context)))
   #{ \markup \bar-numbers #barnum #offsets-lists #}))

%% FIXME: for some reason, when the last bar is an alternative and the
%% Bar_numbers_in_instrument_name_engraver is in use, the last bar number gets
%% printed. Not very annoying, but oddly specific.

#(define (Bar_numbers_in_instrument_name_engraver context)
  (make-engraver
   (listeners
    (
      (StreamEvent engraver event)
      (let* ((barnum (partial-aware-bar-number context))
             (offsets-lists (list-repeat-offsets-lists context))
             (markup #{ \markup \bar-numbers #barnum #offsets-lists #}))
       (ly:context-set-property! context 'instrumentName markup)
       (ly:context-set-property! context 'shortInstrumentName markup))
    ))))

#(ly:register-translator
  Bar_numbers_in_instrument_name_engraver 'Bar_numbers_in_instrument_name_engraver
  '((grobs-created . ())
    (events-accepted . ())
    (properties-read . (currentBarNumber measurePosition))
    (properties-written . (instrumentName))
    (description . "Places the column of bar numbers in the instrument name property")))

\layout {
  \context {
    \Score
    %% barNumberFormatter = #bar-numbers-formatter
    %% barNumberVisibility = #all-bar-numbers-visible
    %% \override BarNumber.break-visibility = ##(#t #t #t)
    \remove "Bar_number_engraver"
  }

  \context {
    \Staff
    \consists "Bar_numbers_in_instrument_name_engraver"
    \override InstrumentName.font-size = #-2
    \override InstrumentName.padding = #1
  }
}
