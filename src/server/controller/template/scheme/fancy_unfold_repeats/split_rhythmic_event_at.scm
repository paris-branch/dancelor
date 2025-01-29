;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;  split-rhythmic-event-at music position                                    ;;
;;                                                                            ;;
;;  Crawls the given music to find if there is a rhythmic event overlapping   ;;
;;  the given position. If there is, split the event into two events with a   ;;
;;  tie in between. For instance, if music is a4 and position is 1/8, then    ;;
;;  this returns a8~a8.                                                       ;;

(define (split-rhythmic-event-at music position)
  (assert-singleton (srea (ly:music-deep-copy music) position)))

(define (srea-rhythmic-event music position)
  (let ((length    (length-of-music music))
        (fst-music (ly:music-deep-copy music))
        (snd-music (ly:music-deep-copy music)))
    (music-set-length! fst-music position)
    (music-add-last-articulation! fst-music (make-music 'TieEvent))
    (music-set-length! snd-music (ly:moment-sub length position))
    (list fst-music snd-music)))

(define (srea-event-chord music position)
  (let ((length    (length-of-music music))
        (fst-music (ly:music-deep-copy music))
        (snd-music (ly:music-deep-copy music)))
    (chord-set-length! fst-music position)
    (music-add-last-element! fst-music (make-music 'TieEvent))
    (chord-set-length! snd-music (ly:moment-sub length position))
    (list fst-music snd-music)))

;; Auxiliary function that tries to split inside the given music event and
;; returns a list of events.
(define (srea music position)

  ;; If the position is zero or outside of the given music, then there is
  ;; nothing to do here and we can just return the music alone.
  (let ((length (length-of-music music)))
    (if (or (ly:moment<=? position ly:moment-zero) (ly:moment<=? length position))
        (list music)

        ;; If the given music is a rhythmic event or an event chord, then we
        ;; call their special handler.
        (if (music-is-of-type? music 'rhythmic-event)
            (srea-rhythmic-event music position)

            (if (music-is-of-type? music 'event-chord)
                (srea-event-chord music position)

                ;; If the given music is simultaneous, then we try to split all
                ;; the sub-elements in the same way. Otherwise, we try to split
                ;; in the sub-elements of the event. These sub functions work in
                ;; place, so we just return the music later. If the position is
                ;; too big, there is possibly no modifications done.
                (begin
                  (srea-sub! music position)
                  (if (music-is-of-type? music 'simultaneous-music)
                      (srea-subs-sim! music position)
                      (srea-subs-seq! music (ly:moment-sub position (length-of-sub-element music))))
                  (list music)))))))

;; Auxiliary function to split inside the sub-element of a music.
(define (srea-sub! music position)
  (let ((element (ly:music-property music 'element)))
    (if (ly:music? element)
        (let* ((element (srea element position))
               (element (assert-singleton element)))
          (ly:music-set-property! music 'element element)))))

;; Auxiliary function to split inside the sub-elements of a simultaneous music.
(define (srea-subs-sim! music position)
  (let* ((elements (ly:music-property music 'elements))
         (elements (map (lambda (element) (srea element position)) elements))
         (elements (flatten elements)))
    (ly:music-set-property! music 'elements elements)))

;; Auxiliary function to split inside the sub-elements of a sequential music.
(define (srea-subs-seq! music position)
  (let* ((elements (ly:music-property music 'elements))
         (elements (srea-list-seq '() elements position)))
    (ly:music-set-property! music 'elements elements)))

(define (srea-list-seq acc musics position)

  ;; If there are no more events in the list to process, then we just return the
  ;; accumulator. This is true whether or not position is zero. (If it is, then
  ;; the cut is already good. If the position is bigger than zero, then the cut
  ;; is bad and we cannot do anything.
  (if (null? musics)
      (reverse acc)

      ;; Otherwise, we take the first element, we try inside that one and we try
      (let* ((music    (car musics))
             (acc      (append (reverse (srea music position)) acc))
             (musics   (cdr musics))
             (position (ly:moment-sub position (length-of-music music))))
        (srea-list-seq acc musics position))))
