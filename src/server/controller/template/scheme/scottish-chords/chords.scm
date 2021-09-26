(define (rewrite-chord-bass pitch)
  (let* ((note   (ly:pitch-notename pitch))
         (alter  (ly:pitch-alteration pitch))
         (octave (if (<= note 2) -1 -2)))
    (ly:make-pitch octave note alter)))

(define (rewrite-chord-note pitch)
  (let* ((note   (ly:pitch-notename pitch))
         (alter  (ly:pitch-alteration pitch))
         (octave (if (<= note 2) 0 -1)))
    (ly:make-pitch octave note alter)))

(define (rewrite-chord-bass-fifth-and-notes chord)

  (let* ((notes    (event-chord-pitches  chord))
         (duration (event-chord-duration chord))

         ;; The bass of the chord is the first note's pitch. We rewrite to make
         ;; it low enough.
         (bass (car notes))
         (bass (rewrite-chord-bass bass))

         ;; Compute the fifth of the bass.
         (fifth (caddr notes))
         (fifth (rewrite-chord-bass fifth))

         ;; We rewrite the chord by rewriting every note in it and rebuilding
         ;; the chord.
         (notes (map rewrite-chord-note notes)))

    ;; We return the pair of the bass and the notes, aka a list starting with
    ;; the bass followed by the notes.
    (cons bass (cons fifth notes))))

(define (rewrite-chords rewrite-chord chordline? music position)

  ;; The argument chordline? is a boolean stating whether we are exactly in a
  ;; chordline yet of if we're still in mixed music. This is to avoid trying to
  ;; rewrite chords in the melody. When we find a wrapper that contains only
  ;; chords, then we can set chordline? to true which will allow the
  ;; modification of chords subsequently.
  (if (and (not chordline?)
           (music-is-of-type? music 'music-wrapper-music)
           (only-chords? music))
      (rewrite-chords rewrite-chord #t music position)

  ;; If music is a chord (and we're in a chordline), then we call
  ;; 'rewrite-chord', which returns a list of chords in the position of the
  ;; given one.
  (if (and (music-is-of-type? music 'event-chord) chordline?)
      (let ((chords (rewrite-chord music position)))
        chords)

      ;; Otherwise, if music is simultaneous, we apply to all the sub-elements
      ;; in parallel.
      (if (music-is-of-type? music 'simultaneous-music)
          (let* ((sub (ly:music-property music 'element))
                 (sub (if (ly:music? sub)
                          (assert-singleton (rewrite-chords rewrite-chord chordline? sub position))
                          sub))

                 (subs (ly:music-property music 'elements))
                 (subs (map (lambda (sub) (rewrite-chords rewrite-chord chordline? sub position)) subs))
                 (subs (map assert-singleton subs)))
            (ly:music-set-property! music 'element sub)
            (ly:music-set-property! music 'elements subs)
            (list music))

          ;; Else
          (begin
            (let ((sub (ly:music-property music 'element)))
              (if (ly:music? sub)
                  (let* ((sub (rewrite-chords rewrite-chord chordline? sub position))
                         (sub (assert-singleton sub))
                         (duration (length-of-music sub))
                         (new-position (ly:moment-add position duration)))
                    (set! position new-position)
                    (ly:music-set-property! music 'element sub))))

            (let* ((subs (ly:music-property music 'elements))
                   (subss (map
                           (lambda (sub)
                             (let* ((subs (rewrite-chords rewrite-chord chordline? sub position))
                                    (durations (map length-of-music subs))
                                    (new-position (fold-left ly:moment-add position durations)))
                               (set! position new-position)
                               subs))
                           subs))
                   (subs (apply append subss)))
              (ly:music-set-property! music 'elements subs))

            (list music))))))

(define (chords rewrite-chord music)
  (let* ((partial-duration (get-partial-length music))
         (position (ly:moment-sub ly:moment-zero partial-duration)))
    (assert-singleton (rewrite-chords rewrite-chord #f music position))))
