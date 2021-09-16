
;; Compute the total duration or length of a music event.

(define (duration-of-music music)
  (duration-of-length (length-of-music music)))

(define (length-of-music music)

  ;; We check whether the music object is a rhythmic event. In that case, we
  ;; simply return its duration.
  (if (music-is-of-type? music 'rhythmic-event)
      (ly:duration-length (ly:music-property music 'duration))

      ;; Otherwise, compute the lengths of the sub-elements.
      (let* ((length   (length-of-sub-element music))
             (elements (ly:music-property music 'elements))
             (lengths  (map length-of-music elements)))

        ;; Check whether the music object isn't a simultaneous object. If the
        ;; music is simultaneous, we return the *max* of all the lengths.
        ;; Otherwise, we return the sum.
        (if (music-is-of-type? music 'simultaneous-music)
            (fold-left ly:moment-max length lengths)
            (fold-left ly:moment-add length lengths)))))

(define (length-of-musics musics)
  (fold-left ly:moment-add ly:moment-zero (map length-of-music musics)))

(define (duration-of-sub-element music)
  (duration-of-length (length-of-sub-element music)))

(define (length-of-sub-element music)
  (let ((element (ly:music-property music 'element)))
    (if (ly:music? element)
        (length-of-music element)
        ly:moment-zero)))
