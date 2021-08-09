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
