(define (ly:music-property-all-elements music)
  (let ((element (ly:music-property music 'element))
        (elements (ly:music-property music 'elements)))
    (if (ly:music? element) (cons element elements) elements)))

(define (ly:moment<? m1 m2)
  (< (ly:moment-main-numerator (ly:moment-sub m1 m2)) 0))

(define (ly:moment<=? m1 m2)
  (<= (ly:moment-main-numerator (ly:moment-sub m1 m2)) 0))

(define (ly:moment=? m1 m2)
  (= (ly:moment-main-numerator (ly:moment-sub m1 m2)) 0))

(define (ly:compare-moments m1 m2)
  (if (ly:moment<? m1 m2) 'Lower (if (ly:moment=? m1 m2) 'Equal 'Greater)))

(define ly:moment-zero (ly:make-moment 0 0))

(define (ly:moment-is-zero? m)
  (= (ly:moment-main-numerator m) 0))

(define (ly:moment-max m1 m2)
  (if (ly:moment<? m1 m2) m2 m1))

(define (event-chord-duration chord)
  (let* ((notes (event-chord-notes chord))
         (note  (car notes)))
    (ly:music-property note 'duration)))

(define (make-note-event pitch duration)
  (make-music 'NoteEvent 'pitch pitch 'duration duration))

(define (music-set-duration! music duration)
  (let ((prev-duration (ly:music-property music 'duration)))
    (if (null? prev-duration)
        (ly:error "music-set-duration!: cannot set duration on an object that do not already have one")
        (ly:music-set-property! music 'duration duration))))

(define (music-set-length! music length)
  (music-set-duration! music (make-duration-of-length length)))

(define (chord-set-duration! chord duration)
  (let* ((notes (ly:music-property chord 'elements))
         (notes (filter (lambda (note) (music-is-of-type? note 'rhythmic-event)) notes)))
    (map (lambda (note) (music-set-duration! note duration)) notes)))

(define (chord-set-length! chord length)
  (chord-set-duration! chord (make-duration-of-length length)))

(define (music-add-last-element! music element)
  (let ((elements (ly:music-property music 'elements)))
    (ly:music-set-property! music 'elements (append elements (list element)))))

(define (music-add-last-articulation! music articulation)
  (let ((articulations (ly:music-property music 'articulations)))
    (ly:music-set-property! music 'articulations (append articulations (list articulation)))))
