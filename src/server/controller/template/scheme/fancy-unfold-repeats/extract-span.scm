;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                            ;;
;;   extract-span music start duration                                        ;;
;;                                                                            ;;
;;   Returns a list whose first element is a string behaving as a             ;;
;;   "constructor". The constructor can be:                                   ;;
;;                                                                            ;;
;;   - [Impossible], if there is no way to find a clean way to cut the        ;;
;;     required span in the given [music] object.                             ;;
;;                                                                            ;;
;;   - [Extracted], if the extraction succeeded. In this case, the list       ;;
;;     comprises two more elements. The second element is the list of         ;;
;;     elements found in the span. The third element is a lambda expecting    ;;
;;     a list of music objects as argument, and rebuilding the full music     ;;
;;     object. For instance, giving the second element of the list as         ;;
;;     argument to the lambda rebuilds the given music object. The point is   ;;
;;     of course to provide a different list of music objects.                ;;

(define (extract-span music start duration)
  (es music start (ly:moment-add start duration)))

;; Auxiliary function that seeks the right place in the music tree, considering
;; the given music object.
(define (es music start end)

  ;; If the starting point is strictly below zero, then it's impossible.
  (if (ly:moment<? start ly:moment-zero)
      '(Impossible)

      ;; Otherwise, the starting point is non-negative. We compare the length of
      ;; the given music to the end of the span. If the length is lower than the
      ;; end, then it's impossible.
      (let ((length (length-of-music music)))
        (if (ly:moment<? length end)
            '(Impossible)

            ;; Otherwise, the span fits in the given music. We try to go in the
            ;; sub-elements by passing the given music to es-seek-sub.
            (es-sub music start end)))))

;; Auxiliary function that seeks the right place in the music tree, considering
;; the sub-element of the given music object. If it does not work in the
;; sub-element, then we go to the sub-elements.
(define (es-sub music start end)

  (let ((sub (ly:music-property music 'element)))
    (if (ly:music? sub)

        (let ((result (es sub start end)))
          (case (car result)

            ;; If it doesn't work, then we try in the sub-elements, taking into
            ;; account the length of the sub-element.
            ((Impossible)
             (let* ((length (length-of-music sub))
                    (start  (ly:moment-sub start length))
                    (end    (ly:moment-sub end   length)))
               (es-subs music start end)))

            ;; Otherwise, it worked, and we only have to rebuild the current
            ;; music, given the builder of the sub-element.
            ((Extracted)
             (es-rebuild result (lambda (sub) (music-clone music 'element sub))))))

        ;; If the sub-element does not exist, then we try in the sub-elements.
        (es-subs music start end))))

;; Auxiliary function that seeks the right place in the music tree, considering
;; the sub-elements of the given music object. If it does not work in the
;; sub-elements, then we fail.
(define (es-subs music start end)

  ;; Get the sub-elements, call es-list on them and analyse the result. If the
  ;; result is impossible, then we fail.
  (let* ((subs   (ly:music-property music 'elements))
         (result (es-list subs start end)))
    (case (car result)
      ((Impossible) '(Impossible))

      ;; Otherwise, it work, and we only have to rebuild the current music,
      ;; given the builder of the sub-elements.
      ((Extracted)
       (es-rebuild result (lambda (subs) (music-clone music 'elements subs)))))))

(define (es-list musics start end)
  (es-list-seek '() musics start end))

(define (es-list-seek before musics start end)

  ;; If start is zero, then it's a job for es-list-here.
  (if (ly:moment-is-zero? start)
      (let ((result (es-list-here '() musics end)))
        (es-rebuild-maybe result (lambda (musics) (append (reverse before) musics))))

      ;; Otherwise, we check whether there are music objects in the given list.
      ;; If there are none, then it is impossible.
      (if (null? musics)
          '(Impossible)

          ;; If there are, we try to fit the span in the first one.
          (let ((music (car musics)) (musics (cdr musics)))
            (if (ly:moment-is-zero? (length-of-musics before))
                (es-list-seek-first before music musics start end)
                (es-list-seek-crawl before music musics start end))))))

(define (es-list-seek-first before music musics start end)
  (let ((result (es music start end)))
    (case (car result)
      ((Extracted)
       (es-rebuild result (lambda (music) (append (reverse before) (cons music musics)))))
      ((Impossible) (es-list-seek-crawl before music musics start end)))))

(define (es-list-seek-crawl before music musics start end)
  (let ((length (length-of-music music)))
    (if (ly:moment<=? length end)
        (es-list-seek (cons music before) musics (ly:moment-sub start length) (ly:moment-sub end length))
        '(Impossible))))

(define (es-list-here before musics end)

  ;; If end is exactly zero, then we are done here.
  (if (ly:moment-is-zero? end)
      (list
       'Extracted
       (reverse before)
       (lambda (before) (append (reverse before) musics)))

      (if (null? musics)
          '(Impossible)

          (let ((music (car musics)) (musics (cdr musics)))
            (if (ly:moment-is-zero? (length-of-musics before))
                (es-list-here-first before music musics end)
                (es-list-here-crawl before music musics end))))))

(define (es-list-here-first before music musics end)

  ;; We try to fit the span in the first element entirely. If it succeed, we are
  ;; done and we only rebuild the list.
  (let ((result (es music ly:moment-zero end)))
    (case (car result)
      ((Extracted)
       (es-rebuild result (lambda (music) (append (reverse before) (cons music musics)))))

      ;; Otherwise, it did not work, so we simply crawl through the current list.
      ((Impossible) (es-list-here-crawl before music musics end)))))

(define (es-list-here-crawl before music musics end)
  (let ((length (length-of-music music)))
    (if (ly:moment<=? length end)
        (es-list-here (cons music before) musics (ly:moment-sub end length))
        '(Impossible))))

(define (es-rebuild result builder)
  (case (car result)
    ((Extracted)
     (list
      'Extracted
      (cadr result)
      (lambda (replacement) (builder ((caddr result) replacement)))))
    (else (ly:error "extract-span: should not happen (call of es-rebuild on non-Extracted)"))))

(define (es-rebuild-maybe result builder)
  (case (car result)
    ((Extracted) (es-rebuild result builder))
    ((Impossible) '(Impossible))))






;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                            ;;
;;   extract-span-from-list                                                   ;;
;;                                                                            ;;
;;   Simplified version.                                                      ;;

(define (extract-span-from-list-acc acc elements span-duration)

  (if (ly:moment-is-zero? span-duration)
      (cons (reverse acc) elements)

      (if (null? elements)
          '()

          (let* ((first  (car elements))
                 (length (length-of-music first)))
            (case (ly:compare-moments length span-duration)
              ((Greater) '())

              (else
               (extract-span-from-list-acc
                (cons first acc) (cdr elements) (ly:moment-sub span-duration length))))))))

(define (extract-span-from-list elements span-duration)
  (extract-span-from-list-acc '() elements span-duration))
