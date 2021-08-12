;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                            ;;
;;   add-total-durations music                                                ;;
;;                                                                            ;;
;;   Modifies in place the given piece of music. Sets the property            ;;
;;   'niols-total-duration' to all the sub-objects of the given music.        ;;
;;   This property contains the raw duration of the object. For a note, it    ;;
;;   is the duration of the note. For other kind of music, it is the          ;;
;;   addition of all the music they contain. For simultaneous music, it is    ;;
;;   the duration of the longest piece of music it contains. Note that this   ;;
;;   the duration of the actual piece, as there can be repeats, etc.          ;;

(define (add-total-durations music)

  (let*

      ;; Crawl through the sub-elements and call recursively on them. This
      ;; returns their durations as moments, which we can process later.
      ((sub-elements  (ly:music-property-all-elements music))
       (sub-durations (map add-total-durations sub-elements))

       ;; Compute the duration of the current element.
       (curr-duration

        ;; If music is a simultaneous music, we check that all the sub-durations
        ;; are the same (FIXME). They must exist.
        (if (music-is-of-type? music 'simultaneous-music)
            (car sub-durations)

            ;; Otherwise, if music is a rhythmic event (aka a note), we check
            ;; that there are no sub-durations (FIXME) and we return the
            ;; duration from the music properties.
            (if (music-is-of-type? music 'rhythmic-event)
                (ly:duration-length (ly:music-property music 'duration))

                ;; Otherwise, we add the sub-durations to get the total
                ;; duration.
                (fold-left ly:moment-add ly:moment-zero sub-durations))))

       ;; Now as an actual duration object.
       (actual-curr-duration (make-duration-of-length curr-duration)))

    ;; Now we set it, but we return the length only.
    (ly:music-set-property! music 'niols-total-duration actual-curr-duration)
    curr-duration))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                            ;;
;;   get-total-duration music                                                 ;;
;;   get-total-length music                                                   ;;
;;                                                                            ;;
;;   Read the field 'niols-total-duration and return it either as a           ;;
;;   duration object (for get-total-duration) or as a moment                  ;;
;;   (for get-total-length). This requires that the function                  ;;
;;   add-total-durations has been called beforehand, otherwise this field     ;;
;;   will not be set.                                                         ;;

(define (get-total-duration music)
  (ly:music-property music 'niols-total-duration))

(define (get-total-length music)
  (ly:duration-length (get-total-duration music)))
