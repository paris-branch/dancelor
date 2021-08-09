;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                            ;;
;;   unfold-first-volta-repeat      duration             music                ;;
;;   unfold-first-volta-repeat-sub  duration             music                ;;
;;   unfold-first-volta-repeat-list duration musics-done musics               ;;
;;                                                                            ;;
;;   Return a list. The first element of the list is the potentially          ;;
;;   modified music in which the first volta has been unfolded. The second    ;;
;;   element of the list is either 'NoVolta or 'FoundVolta, depending on      ;;
;;   whether a volta has been found or not. In case of 'NoVolta, the list     ;;
;;   contains a third element, a moment representing the duration up to the   ;;
;;   last bit of music included. In case of 'FoundVolta, the list contains    ;;
;;   an additional integer and several moments. The third element of the      ;;
;;   list is the duration up to the beginning of the volta. The fourth        ;;
;;   element of the list is the number of repetitions in the volta. The       ;;
;;   fifth and subsequent elements are the duration of the the volta,         ;;
;;   starting with the duration of the body, followed by the duration of      ;;
;;   alternatives, if there are any.                                          ;;
;;                                                                            ;;
;;   These functions are mutually recursive. The list version takes an        ;;
;;   accumulator for efficiency. Instead of returning a music object as       ;;
;;   first element of the pair, it returns a list of music objects. The       ;;
;;   sub version takes a music object but applies only on its sub-elements.   ;;
;;   It could be inline in find-first-volta-repeat, but is not for            ;;
;;   readability. These three functions otherwise return similar values.      ;;

(define (unfold-first-volta-repeat-list duration musics-done musics)

  ;; If there are no more musics to handle, then we are done. We have not found
  ;; any volta repeat, so we return a pair containing the list of musics done
  ;; and the duration of the whole thing.
  (if (null? musics)
      (list (reverse musics-done) 'NoVolta duration)

      ;; Otherwise, we try to unfold volta repeats in the first music to handle.
      ;; The result is a pair containing the potentially modified music and
      ;; either one or several durations.
      (let ((result      (unfold-first-volta-repeat duration (car musics))))

        ;; If we did find a volta repeat, we just reconstruct the list properly
        ;; and we are done.
        (if (equal? (cadr result) 'FoundVolta)
            (cons
             (append (reverse musics-done) (cons (car result) (cdr musics)))
             (cdr result))

            ;; Otherwise, it is only the duration up to know. We keep going.
            (let ((duration (caddr result)))
              (unfold-first-volta-repeat-list duration (cons (car musics) musics-done) (cdr musics)))))))

(define (unfold-first-volta-repeat-sub duration music)

  ;; If there is actually a first element that is music, we try to run the
  ;; function on it. Otherwise, we dummily return that we have found nothing.
  (let* ((element  (ly:music-property music 'element))
         (result   (if (ly:music? element)
                       (unfold-first-volta-repeat duration element)
                       (list element 'NoVolta duration))))

    ;; If a volta repeat has been found, we reconstruct the music object.
    (if (equal? (cadr result) 'FoundVolta)
        (cons (music-clone music 'element (car result)) (cdr result))

        ;; Otherwise, we continue on the sub-elements from the duration after
        ;; the sub-element that has been processed.
        (let* ((duration (caddr result))
               (elements (ly:music-property music 'elements))
               (result   (unfold-first-volta-repeat-list duration '() elements)))

          ;; If a volta repeat has been found, we reconstruct the music object.
          (if (equal? (cadr result) 'FoundVolta)
              (cons (music-clone music 'elements (car result)) (cdr result))

              ;; Otherwise, no volta-repeat has been found and we return.
              (cons music (cdr result)))))))

(define (unfold-first-volta-repeat duration music)

  ;; If the music is simultaneous, we handle only the first branch. We return a
  ;; cloned version of the music object in which we insert the modified
  ;; elements. This is to avoid modifying the music elements in place:
  ;; otherwise, we could not just try several branches in a future
  ;; implementation.
  (if (music-is-of-type? music 'simultaneous-music)
      (let* ((elements (ly:music-property music 'elements))
             (result   (unfold-first-volta-repeat duration (car elements))))
        (cons (music-clone music 'elements (cons (car result) (cdr elements))) (cdr result)))

      ;; Otherwise, if the music is a note (or any other rhythmic event), then
      ;; we for sure will not find a volta repeat in it. We return the music
      ;; unchanged and we just add the duration of the note to the given one.
      (if (music-is-of-type? music 'rhythmic-event)
          (let ((event-duration (ly:duration-length (ly:music-property music 'duration))))
            (list music 'NoVolta (ly:moment-add duration event-duration)))

          ;; Otherwise, if the music is a volta repeat, then it is the first one
          ;; we ever encountered. We return its unfolded version as well as the
          ;; duration of its various sub-elements.
          (if (music-is-of-type? music 'volta-repeated-music)
              (append
               (list
                (make-music 'UnfoldedRepeatedMusic music)
                'FoundVolta duration (ly:music-property music 'repeat-count))
               (map get-total-length (ly:music-property-all-elements music)))

              ;; Finally, if the music is none of these, we crawl through its
              ;; sub-elements until we find something.
              (unfold-first-volta-repeat-sub duration music)))))
