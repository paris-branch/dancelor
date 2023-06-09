(define (rewrite-jig-chord-aux first? bass fifth notes duration position)

  (let* ((position (ly:moment-mod position (ly:make-moment 6 8)))

         (fourth   (ly:make-moment 1 4))
         (fourth-duration (duration-of-length fourth))
         (eigth    (ly:make-moment 1 8))
         (eigth-duration (duration-of-length eigth))

         (bass-chord (make-chord-of-pitches (list bass) fourth-duration))
         (fifth-chord (make-chord-of-pitches (list fifth) fourth-duration))

         (chord (make-chord-of-pitches notes eigth-duration))
         (chord-with-bass (make-chord-of-pitches (cons bass notes) eigth-duration))
         (chord-with-bass-and-original-duration
          (make-chord-of-pitches (cons bass notes) (duration-of-length duration))))

    ;; If we are shorter than an eigth, then we don't know what to do. Either
    ;; it's zero and we return an empty list, or it's not and we return the
    ;; chord as it is (rewritten).
    (if (ly:moment<=? duration eigth)
        (if (ly:moment-is-zero? duration) '() (list chord-with-bass-and-original-duration))

        ;; If beginning of the bar, then just bass.
        (if (ly:moment-is-zero? position)
            (let* ((duration      (ly:moment-sub duration fourth))
                   (next-position (ly:moment-add position fourth))
                   (other-chords  (rewrite-jig-chord-aux bass #f fifth notes duration next-position)))
              (cons bass-chord other-chords))

            ;; If third beat, then fifth, except when that's the first
            ;; occurrence of the chord, in which case we put the bass.
            (if (ly:moment=? position (ly:make-moment 3 8))
                (let* ((duration      (ly:moment-sub duration fourth))
                       (next-position (ly:moment-add position fourth))
                       (other-chords  (rewrite-jig-chord-aux bass #f fifth notes duration next-position)))
                  (cons (if first? bass-chord fifth-chord) other-chords))

                ;; Otherwise, chord. When that's the first occurrence of the
                ;; chord, we also add the bass to it.
                (let* ((duration      (ly:moment-sub duration eigth))
                       (next-position (ly:moment-add position eigth))
                       (other-chords  (rewrite-jig-chord-aux bass #f fifth notes duration next-position)))
                  (cons (if first? chord-with-bass chord) other-chords)))))))

(define (rewrite-jig-chord chord position)
  (let* ((bass-fifth-and-notes (rewrite-chord-bass-fifth-and-notes chord))
         (bass     (car  bass-fifth-and-notes))
         (fifth    (cadr bass-fifth-and-notes))
         (notes    (cddr bass-fifth-and-notes))
         (duration (ly:duration-length (event-chord-duration chord))))
    (rewrite-jig-chord-aux #t bass fifth notes duration position)))

(define (jig-chords music) (chords rewrite-jig-chord music))
