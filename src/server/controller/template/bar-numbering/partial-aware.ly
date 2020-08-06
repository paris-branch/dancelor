\layout {
  \context {
    \Score

    %% Define an other barNumberFormatter. This one prints broken bars as if
    %% they were already in the next bar. It only makes sense at the beginning
    %% of a line, for scottish dancing, for instance. (FIXME: find a better
    %% name)
    #(define-public (partial-aware-bar-number-formatter barnum measure-pos alt-number context)
      (let* ((begin-measure (= 0 (ly:moment-main-numerator measure-pos)))
             (broken-bar (and (not begin-measure) (!= barnum 1)))
             (barnum (+ barnum (if broken-bar 1 0))))
       (markup (number->string barnum))))
    barNumberFormatter = #partial-aware-bar-number-formatter
  }
}
