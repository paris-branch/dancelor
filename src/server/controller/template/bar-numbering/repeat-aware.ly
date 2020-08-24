
%% ============================= [ Utilities ] ============================== %%

#(define (create-context-property name type doc)
  (set-object-property! name 'translation-type? type)
  (set-object-property! name 'translation-doc doc)
  (set! all-translation-properties (cons name all-translation-properties))
  (set! all-user-translation-properties (cons name all-user-translation-properties)))

#(define (box x) (list x))
#(define (unbox b) (car b))
#(define (set-box! b x) (set-car! b x))

#(define (mapi f l) (map f (iota (length l)) l))
#(define (flatten ll) (apply append ll))
#(define (nth l n) (if (null? l) '() (if (<= n 0) (car l) (nth (cdr l) (- n 1)))))

#(define (truncate l n) (if (null? l) '() (if (<= n 0) '() (cons (car l) (truncate (cdr l) (- n 1))))))

%% ====================== [ Current Repeat Intervals ] ====================== %%

#(create-context-property 'currentRepeatOffsets list? "offsets of repeats in which we are")

#(define (current-repeat-offsets context)
  (ly:context-property context 'currentRepeatOffsets (list (box 0))))

#(define (set-current-repeat-offsets! context cro)
  (ly:context-set-property! context 'currentRepeatOffsets cro))

#(define (new-repeat-offsets-layer size)
  (map (lambda (n) (box 0)) (iota size)))

#(define (add-repeat-offsets-layer-aux cro offsets)
  (cons (car cro)
   (if (null? (cdr cro))
    (map (lambda (e) (cons e '())) offsets)
    (map (lambda (c) (add-repeat-offsets-layer-aux c offsets)) (cdr cro)))))

#(define (add-repeat-offsets-layer! context offsets)
  (let* ((cro (current-repeat-offsets context))
         (cro (add-repeat-offsets-layer-aux cro offsets)))
   (set-current-repeat-offsets! context cro)))

#(define (remove-repeat-offsets-layer-aux cro)
  (if (null? (cdr cro))
   '()
   (let ((cdr-cro (map (lambda (c) (remove-repeat-offsets-layer-aux c)) (cdr cro))))
    (cons (car cro)
     (if (null? (car cdr-cro)) ;; assume they are then all null because complete tree
      '()
      cdr-cro)))))

#(define (remove-repeat-offsets-layer! context)
  (let* ((cro (current-repeat-offsets context))
         (cro (remove-repeat-offsets-layer-aux cro)))
   (set-current-repeat-offsets! context cro)))

#(define (truncate-repeat-offsets-layer-aux cro length)
  (if (null? (cdr cro))
   '()
   (let ((cdr-cro (map (lambda (c) (truncate-repeat-offsets-layer-aux c length)) (cdr cro))))
    (cons (car cro)
     (if (null? (car cdr-cro)) ;; assume they are then all null because complete tree
      (truncate (cdr cro) length)
      cdr-cro)))))

#(define (truncate-repeat-offsets-layer! context length)
  (let* ((cro (current-repeat-offsets context))
         (cro (truncate-repeat-offsets-layer-aux cro length)))
   (set-current-repeat-offsets! context cro)))

#(define (list-repeat-offsets-lists-aux cro)
  (if (null? (cdr cro))
   (list cro)
   (let* ((offsets-lists-list-list (map (lambda (c) (list-repeat-offsets-lists-aux c)) (cdr cro)))
          (offsets-lists-list      (flatten offsets-lists-list-list))
          (offsets-lists-list      (map (lambda (o) (cons (car cro) o)) offsets-lists-list)))
    offsets-lists-list)))

#(define (list-repeat-offsets-lists context)
  (list-repeat-offsets-lists-aux (current-repeat-offsets context)))

#(define (debug-display-repeat-offsets context)
  (display "currentRepeatOffsets = ")
  (display (ly:context-property context 'currentRepeatOffsets))
  (newline))

#(define (debug-display-repeat-offsets-lists context)
  (display "repeat-offsets-lists =\n")
  (map (lambda (repeat-offsets-list) (display "  - ")(display repeat-offsets-list)(newline)) (list-repeat-offsets-lists context)))

#(define (partial-aware-bar-number context)
  (let* ((cbn (ly:context-property context 'currentBarNumber))
         (mp  (ly:context-property context 'measurePosition))
         (mp  (if (null? mp) 0 (ly:moment-main-numerator mp))))
   (if (> mp 0)
    (set! cbn (+ cbn 1)))
   cbn))

#(define (set-partial-aware-bar-number! context number)
  (let* ((mp  (ly:context-property context 'measurePosition))
         (mp  (ly:moment-main-numerator mp)))
   (if (> mp 0)
    (set! number (- number 1)))
   (ly:context-set-property! context 'currentBarNumber number)))

#(define (context-spec-music-applied-to-score f)
  (context-spec-music (make-apply-context f) 'Score))

#(define (new-volta-set music)
  (let* ((elt (ly:music-property music 'element))
         (alts (ly:music-property music 'elements))
         (lalts (length alts))
         (rep-count (ly:music-property music 'repeat-count))
         (occ-first-repeat (1+ (- rep-count lalts)))
         (start-barnum '())
         (duration-common '())
         (offsets (new-repeat-offsets-layer rep-count)))
   (ly:music-set-property! music 'element
    (make-sequential-music
     (list

      ;; Before the body of the repeat, add an element that will save the bar number
      ;; at that point and add the right number of repeat offsets to be filled.
      (context-spec-music-applied-to-score
       (lambda (context)
        (set! start-barnum (partial-aware-bar-number context))
        (add-repeat-offsets-layer! context offsets)))

      ;; If this is the first bar, then force the apparition of a repeat bar.
      (context-spec-music-applied-to-score
       (lambda (context)
        (let ((barnum (partial-aware-bar-number context))
              (start-repeat-type (ly:context-property context 'startRepeatType)))
         (if (= barnum 1)
          (ly:context-set-property! context 'whichBar start-repeat-type)))))

      elt

      ;; After the body, and if there are no alternatives, we compute the duration of
      ;; the whole volta and we update the repeat lengths accordingly.
      (if (null? alts)
       (context-spec-music-applied-to-score
        (lambda (context)
         ;; Compute the duration of these repeats. This will be the same for all
         ;; as there is no alternative. and remove the layer from the current
         (let ((duration (- (partial-aware-bar-number context) start-barnum)))
          ;; Fill in all the offsets and remove the layer from the current
          ;; repeat offsets
          (mapi (lambda (i b) (set-box! b (* duration i))) offsets)
          (remove-repeat-offsets-layer! context)
          ;; Bump bar number
          (set-partial-aware-bar-number! context (+ start-barnum (* duration rep-count))))))

       ;; After the body, and if there are alternatives, we compute the common
       ;; duration of all the alternatives, that is the duration before entering
       ;; the alternatives.
       (context-spec-music-applied-to-score
        (lambda (context)
         (set! duration-common (- (partial-aware-bar-number context) start-barnum)))))
    )))

   (mapi (lambda (alt-no alt)
         (make-music
          'SequentialMusic
          'elements
          ;; set properties for proper bar numbering
          (append
           (list (make-music 'AlternativeEvent
                  'alternative-dir (if (= alt-no 0) -1 0)
                  'alternative-increment (if (= 0 alt-no) (1+ (- rep-count lalts)) 1)))

           (list

            ;; For the first alternative, we only keep the number of offsets that are
            ;; actually played inside of it.
            (if (= alt-no 0)
             (context-spec-music-applied-to-score
              (lambda (context)
               (truncate-repeat-offsets-layer! context occ-first-repeat)))
             (make-music 'Music 'void #t))

            ;; Body of the alternative
            alt

            (if (< alt-no (1- lalts)) ;; for all alternative but the last
             (context-spec-music-applied-to-score
              (lambda (context)
               (let* ((barnum           (partial-aware-bar-number context))
                      (duration         (- barnum start-barnum))
                      (new-barnum-first (+ start-barnum (+ (* occ-first-repeat duration) duration-common)))
                      (new-barnum-other (+ barnum duration-common))
                      (new-barnum       (if (= alt-no 0) new-barnum-first new-barnum-other)))
                ;; Fill in the offset/s corresponding to this alternative. If this is the
                ;; first alternative, there might be a bunch of different ones to fill in.
                ;; For the other alternatives, there is only one.
                (if (= alt-no 0)
                 (map (lambda (i)
                       (let* ((i (1+ i))
                              (offset (nth offsets i)))
                        (set-box! offset (* duration i))))
                  (iota occ-first-repeat))
                 (let* ((offset-no (+ alt-no occ-first-repeat))
                        (offset    (nth offsets offset-no)))
                  (set-box! offset (- barnum start-barnum))))
                ;; Bump bar number
                (set-partial-aware-bar-number! context new-barnum))))
             (make-music 'Music 'void #t))

            ;; After the first alternative, we can cleanup the repeat-offsets-layer.
            ;; This works because the bar number is bumped correctly in the alternatives
            ;; already.
           (if (= alt-no 0)
            (context-spec-music-applied-to-score
             (lambda (context)
              (remove-repeat-offsets-layer! context)))
            (make-music 'Music 'void #t)))

           (if (= alt-no (1- lalts))
            (list (make-music 'AlternativeEvent
                   'alternative-dir 1
                   'alternative-increment 0))
            '()))))
    alts)))

#(define the-make-music make-music)

#(define (make-music-wrapped name . music-properties)
  (let ((music (apply the-make-music (cons name music-properties))))
   (if (equal? name 'VoltaRepeatedMusic)
    (ly:music-set-property! music 'elements-callback new-volta-set))
   music))

#(set! make-music make-music-wrapped)

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
