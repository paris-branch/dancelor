(define (rewrite-reel-chord-aux first? bass fifth notes duration position)
  (let* ((position (ly:moment-mod position (ly:make-moment 1 1)))

         (fourth   (ly:make-moment 1 4))
         (fourth-duration (make-duration-of-length fourth))

         (bass-chord (make-event-chord (list (make-note-event bass fourth-duration))))
         (fifth-chord (make-event-chord (list (make-note-event fifth fourth-duration))))

         (chord (map (lambda (note) (make-note-event note fourth-duration)) notes))
         (chord (make-event-chord chord))

         (chord-with-bass (map (lambda (note) (make-note-event note fourth-duration)) (cons bass notes)))
         (chord-with-bass (make-event-chord chord-with-bass)))

    ;; If we are strictly shorter than a fourth, then we don't know what to do.
    ;; Either it's zero and we return an empty list, or it's not and we return
    ;; the chord as it is (rewritten).
    (if (ly:moment<? duration fourth)
        (if (ly:moment-is-zero? duration) '() (list chord))

        ;; Otherwise, there is room to do something with it.
        (begin

          (let* ((duration      (ly:moment-sub duration fourth))
                 (next-position (ly:moment-add position fourth))
                 (other-chords  (rewrite-reel-chord-aux bass #f fifth notes duration next-position)))

            ;; If beginning of the bar, then just bass.
            (if (ly:moment-is-zero? position)
                (cons bass-chord other-chords)

                ;; If third beat, then fifth, except when that's the first
                ;; occurrence of the chord, in which case we put the bass.
                (if (ly:moment=? position (ly:make-moment 1 2))
                    (cons (if first? bass-chord fifth-chord) other-chords)

                    ;; Otherwise, chord. When that's the first occurrence of the
                    ;; chord, we also add the bass to it.
                    (cons (if first? chord-with-bass chord) other-chords))))))))

(define (rewrite-reel-chord chord position)
  (let* ((bass-fifth-and-notes (rewrite-chord-bass-fifth-and-notes chord))
         (bass     (car  bass-fifth-and-notes))
         (fifth    (cadr bass-fifth-and-notes))
         (notes    (cddr bass-fifth-and-notes))
         (duration (ly:duration-length (event-chord-duration chord))))
    (rewrite-reel-chord-aux #t bass fifth notes duration position)))

(define (reel-chords music) (chords rewrite-reel-chord music))
