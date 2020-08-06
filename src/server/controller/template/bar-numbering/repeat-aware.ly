#(define (moment-is-zero? moment)
  (eq? 0 (ly:moment-main-numerator moment)))

#(define (partial-aware-bar-number context)
  (let ((cbn (ly:context-property context 'currentBarNumber))
        (mp  (ly:context-property context 'measurePosition)))
   (if (not (moment-is-zero? mp))
    (set! cbn (+ cbn 1)))
   cbn))

#(define (set-partial-aware-bar-number! context number)
  (let ((mp (ly:context-property context 'measurePosition)))
   (if (not (moment-is-zero? mp))
    (set! number (- number 1)))
   (ly:context-set-property! context 'currentBarNumber number)))

#(define (new-volta-set music)
   (let* ((elt (ly:music-property music 'element))
          (alts (ly:music-property music 'elements))
          (lalts (length alts))
          (rep-count (ly:music-property music 'repeat-count))
          (init-number '())
          (alt-init '()))
     (ly:music-set-property! music 'element
       (make-sequential-music
        (list
         (context-spec-music
          (make-apply-context
           (lambda (context)
             (set! init-number
                   (partial-aware-bar-number context))))
          'Score)
         elt
         (if (null? alts)
             (context-spec-music
              (make-apply-context
               (lambda (context)
                 (set-partial-aware-bar-number! context
                   (+ init-number
                     (* (-
                         (partial-aware-bar-number context)
                         init-number)
                       rep-count)))))
              'Score)
             (make-music 'Music 'void #t))
         )))
     (map (lambda (x y)
            (make-music
             'SequentialMusic
             'elements
             ;; set properties for proper bar numbering
             (append
              (list (make-music 'AlternativeEvent
                      'alternative-dir (if (= y 0)
                                           -1
                                           0)
                      'alternative-increment
                      (if (= 0 y)
                          (1+ (- rep-count
                                lalts))
                          1)))
              (list
               (context-spec-music
                (make-apply-context
                 (lambda (context)
                   (set! alt-init
                         (partial-aware-bar-number context))))
                'Score)
               x
               (if (< y 1)
                   (context-spec-music
                    (make-apply-context
                     (lambda (context)
                       (set-partial-aware-bar-number! context
                         (-
                          (+ init-number
                            (* (-
                                (partial-aware-bar-number context)
                                init-number)
                              rep-count))
                          (- (partial-aware-bar-number context)
                            alt-init)))))
                    'Score)
                   (make-music 'Music 'void #t)))
              (if (= y (1- lalts))
                  (list (make-music 'AlternativeEvent
                          'alternative-dir 1
                          'alternative-increment 0))
                  '()))))
       alts
       (iota lalts))))

#(define the-make-music make-music)

#(define (make-music-wrapped name . music-properties)
  (let ((music (apply the-make-music (cons name music-properties))))
   (if (equal? name 'VoltaRepeatedMusic)
    (ly:music-set-property! music 'elements-callback new-volta-set))
   music))

#(set! make-music make-music-wrapped)
