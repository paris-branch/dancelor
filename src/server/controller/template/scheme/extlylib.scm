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

(define (ly:moment<>? m1 m2)
  (not (ly:moment=? m1 m2)))

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

(define (make-chord-of-pitches pitches duration)
  (make-event-chord (map (lambda (pitch) (make-note-event pitch duration)) pitches)))

;; ;; Same as make-duration-of-length but better on round values.
;; ;; FIXME: And completely buggy so deactivated for now.
;; (define (duration-of-length moment)
;;   (let* ((num  (ly:moment-main-numerator moment))
;;          (den  (ly:moment-main-denominator moment))
;;          (length   (/ (log den) (log 2)))
;;          (dotcount (- (/ (log (+ num 1)) (log 2)) 1)))
;;     (if (and (integer? length) (integer? dotcount))
;;         (ly:make-duration (inexact->exact length) (inexact->exact dotcount))
;;         (ly:make-duration 0 0 num den))))
(define (duration-of-length moment)
  (make-duration-of-length moment))

(define (music-set-duration! music duration)
  (let ((prev-duration (ly:music-property music 'duration)))
    (if (null? prev-duration)
        (ly:error "music-set-duration!: cannot set duration on an object that does not already have one")
        (ly:music-set-property! music 'duration duration))))

(define (music-set-length! music length)
  (music-set-duration! music (duration-of-length length)))

(define (chord-set-duration! chord duration)
  (let* ((notes (ly:music-property chord 'elements))
         (notes (filter (lambda (note) (music-is-of-type? note 'rhythmic-event)) notes)))
    (map (lambda (note) (music-set-duration! note duration)) notes)))

(define (chord-set-length! chord length)
  (chord-set-duration! chord (duration-of-length length)))

(define (music-add-last-element! music element)
  (let ((elements (ly:music-property music 'elements)))
    (ly:music-set-property! music 'elements (append elements (list element)))))

(define (music-add-last-articulation! music articulation)
  (let ((articulations (ly:music-property music 'articulations)))
    (ly:music-set-property! music 'articulations (append articulations (list articulation)))))

;; This function checks whether all the note events appear within a chord. This
;; is used to detect a chord line to modify.
(define (only-chords? music)
  (if (music-is-of-type? music 'event-chord)
      #t
      (if (music-is-of-type? music 'note-event)
          #f
          (let ((elements (ly:music-property-all-elements music)))
            (forall only-chords? elements)))))
