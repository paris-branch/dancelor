%% bar-numbers-formatter, formatting function that takes into account the
%% offsets and write several bar numbers in a column.

#(define-markup-command (apply-offsets-list layout props barnum offsets) (number? list?)
  (let ((barnum (fold (lambda (offset barnum) (+ barnum (unbox offset))) barnum offsets)))
   (interpret-markup layout props
    (markup (number->string barnum)))))

#(define-markup-command (bar-numbers layout props barnum offsets-lists) (number? list?)
  (let ((barnums (map (lambda (offsets) #{ \markup \apply-offsets-list #barnum #offsets #}) offsets-lists)))
   (interpret-markup layout props
    #{ \markup \override #'(baseline-skip . 2) \right-column #barnums #})))

#(define (bar-numbers-formatter barnum measure-pos alt-number context)
  (let ((barnum (partial-aware-bar-number context))
        (offsets-lists (list-repeat-offsets-lists context)))
   #{ \markup \bar-numbers #barnum #offsets-lists #}))
