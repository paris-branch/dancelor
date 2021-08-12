;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                            ;;
;;   contains-span start end span-start span-end                              ;;
;;   element-contains-span start element  span-start span-duration            ;;
;;   elements-contain-span start elements span-start span-duration            ;;
;;                                                                            ;;
;;   Determine whether the span delimited by [start] and [end] contains the   ;;
;;   span delimited by [span-start] and [span-end]. The result is given as    ;;
;;   a string, which can be either:                                           ;;
;;                                                                            ;;
;;   - [SpanOutsideBefore], if the span is contained entirely outside of      ;;
;;     the element, before.                                                   ;;
;;                                                                            ;;
;;   - [SpanOutsideAfter], if the span is contained entirely outside of the   ;;
;;     element, after.                                                        ;;
;;                                                                            ;;
;;   - [SpanHasBadOverlap], if the span overlaps with the element in a bad    ;;
;;     way, eg. by starting before but ending within.                         ;;
;;                                                                            ;;
;;   - [SpanContainedHere], if the span is contained by the element and       ;;
;;     starts exactly here, that is if [current-start = span-start].          ;;
;;                                                                            ;;
;;   - [SpanContainedStrictly], if the span is contained by the element but   ;;
;;     does not start here, that is if [current-start < span-start].          ;;
;;                                                                            ;;
;;   Too more handy versions are proposed: [element-contains-span] and        ;;
;;   [elements-contain-span]. They both take the span as delimited by a       ;;
;;   starting point and a duration. They define the containing span thanks    ;;
;;   to a starting point and an element, or a list of elements.               ;;

(define (contains-span current-start current-end span-start span-end)

  (case (ly:compare-moments current-start span-start)
    ((Greater)
     (case (ly:compare-moments current-start span-end)
       ((Lower) 'SpanHasBadOverlap)
       (else    'SpanOutsideBefore)))

    ((Equal)
     (case (ly:compare-moments current-end span-end)
       ((Lower)
        ;; There is one specific case when the current span is empty. Then we
        ;; want to say 'SpanOutsideAfter and not 'SpanHasBadOverlap.
        (case (ly:compare-moments current-start current-end)
          ((Equal) 'SpanOutsideAfter)
          (else    'SpanHasBadOverlap)))

       (else 'SpanContainedHere)))

    ((Lower)
     (case (ly:compare-moments current-end span-end)
       ((Lower)
        (case (ly:compare-moments current-end span-start)
          ((Greater) 'SpanHasBadOverlap)
          (else      'SpanOutsideAfter)))

       (else 'SpanContainedStrictly)))))

(define (element-contains-span start element span-start span-duration)
  (let* ((end      (ly:moment-add start      (get-total-length element)))
         (span-end (ly:moment-add span-start  span-duration)))
    (contains-span start end span-start span-end)))

(define (elements-contain-span start elements span-start span-duration)
  (let* ((durations (map get-total-length elements))
         (end       (fold-left ly:moment-add start durations))
         (span-end  (ly:moment-add span-start  span-duration)))
    (contains-span start end span-start span-end)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                            ;;
;;   rebuild-if-needed result builder                                         ;;
;;                                                                            ;;
;;   Helper to rebuild. If [result] is an error, then this simply returns     ;;
;;   the error as well. If [result] is an extraction, then [builder] is       ;;
;;   called and given the extraction. The result is used as the new           ;;
;;   extraction function.                                                     ;;

(define (rebuild-if-needed result builder)
  (case (car result)

    ((Extracted)
     (list
      'Extracted (cadr result)
      (lambda (replacement) (builder ((caddr result) replacement)))))

    (else result)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                            ;;
;;   extract-span-seek start music span-start span-duration               ;;
;;   extract-span-seek-list start musics span-start span-duration         ;;
;;                                                                            ;;
;;   Auxiliary function specialised in seeking in a [music] object, or a      ;;
;;   list [musics] of music objects, considering that it starts at moment     ;;
;;   [start]. The goal is to find the first place in [music] that is at       ;;
;;   moment [span-start]. We also check that it is possible for this place    ;;
;;   to contain a span of duration [span-duration] (moment). Once this        ;;
;;   place is reached, we give the rest of the job to one of the *-here-*     ;;
;;   variants to find the proper extraction.                                  ;;
;;                                                                            ;;
;;   Returns a list whose first element is a string behaving as a             ;;
;;   "constructor". The constructor can be:                                   ;;
;;                                                                            ;;
;;   - [ShouldNotHappen], if we entered a case supposed to be impossible.     ;;
;;     This means that there is a bug in the algorithm.                       ;;
;;                                                                            ;;
;;   - [NotPossible], if there is no way to find a clean way to cut the       ;;
;;     required span in the given [music] object.                             ;;
;;                                                                            ;;
;;   - [Extracted], if the extraction succeeded. In this case, the list       ;;
;;     comprises two more elements. The second element is the list of         ;;
;;     elements found in the span. The third element is a lambda expecting    ;;
;;     a list of music objects as argument, and rebuilding the full music     ;;
;;     object. For instance, giving the second element of the list as         ;;
;;     argument to the lambda rebuilds the given music object. The point is   ;;
;;     of course to provide a different list of music objects.                ;;

(define (extract-span-seek start music span-start span-duration)

  ;; FIXME: remove the simultaneous music!
  (if (music-is-of-type? music 'simultaneous-music)
      (display "ALERT: SIMULTANEOUS MUSIC!!!\n"))

  ;; Have a look at how the span is contained in the [music] element.
  (let ((containment (element-contains-span start music span-start span-duration)))
    (case containment

      ((SpanContainedHere)
       (extract-span-here music span-duration))

      ;; If it is contained strictly, then we have to enter either
      ((SpanContainedStrictly)

       (let* ((element     (ly:music-property music 'element))
              (containment (if (ly:music? element)
                               (element-contains-span start element span-start span-duration)
                               'SpanOutsideAfter)))

         (case containment
           ((SpanOutsideBefore) (list 'ShouldNotHappen))
           ((SpanHasBadOverlap) (list 'NotPossible))

           ;; Should not happen but whatever.
           ((SpanContainedHere)
            (rebuild-if-needed
             (extract-span-here element span-duration)
             (lambda (replacement) (music-clone music 'element replacement))))

           ((SpanContainedStrictly)
            (rebuild-if-needed
             (extract-span-seek start element span-start span-duration)
             (lambda (replacement) (music-clone music 'element replacement))))

           ;; If the span is completely contained after the element (or possibly
           ;; if there is no element), then it must be in the elements (plural),
           ;; so we go have a look at them.
           ((SpanOutsideAfter)
            (let* ((element-duration (if (ly:music? element) (get-total-length element) ly:moment-zero))
                   (element-end      (ly:moment-add start element-duration))
                   (elements         (ly:music-property music 'elements)))
              (rebuild-if-needed
               (extract-span-seek-list element-end elements span-start span-duration)
               (lambda (replacement) (music-clone music 'elements replacement))))))))

      (else (list 'NotPossible)))))

(define (extract-span-seek-list start musics span-start span-duration)

  (if (null? musics)
      'ShouldNotHappen

      (let* ((element (car musics)))

        ;; Try to see if the first element could not contain the whole thing.
        (case (element-contains-span start element span-start span-duration)
          ((SpanOutsideBefore) 'ShouldNotHappen)

          ;; Should not happen, because otherwise, we would have called
          ;; extract-span-here on the element, but whatever.
          ((SpanContainedHere)
           (rebuild-if-needed
            (extract-span-here element span-duration)
            (lambda (replacement) (cons replacement (cdr musics)))))

          ;; If the span is contained strictly in the first element, we continue
          ;; seeking there.
          ((SpanContainedStrictly)
           (rebuild-if-needed
            (extract-span-seek start element span-start span-duration)
            (lambda (replacement) (cons replacement (cdr musics)))))

          ;; If the span is contained fully after, we discard the current
          ;; element and go look for the span in the rest of the list.
          ((SpanOutsideAfter)
           (let* ((element-duration (get-total-length element))
                  (element-end      (ly:moment-add start element-duration))
                  (elements         (cdr musics)))
             (rebuild-if-needed
              (extract-span-seek-list element-end elements span-start span-duration)
              (lambda (replacement) (cons element replacement)))))

          ;; If there is a bad overlap, it might be because the element is the
          ;; first of the list that manages to fit the whole thing, so we check
          ;; whether the span would not fit exactly starting with this element.
          ((SpanHasBadOverlap)
           (case (elements-contain-span start musics span-start span-duration)
             ((SpanContainedHere)
              (extract-span-here-list musics span-duration))
             (else 'NotPossible)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                            ;;
;;   extract-span-here music span-duration                                    ;;
;;   extract-span-here-list musics span-duration                              ;;

(define (extract-span-here music span-duration)

  (let* ((element     (ly:music-property music 'element))
         (containment (if (ly:music? element)
                          (element-contains-span ly:moment-zero element ly:moment-zero span-duration)
                          'SpanOutsideAfter)))

    (case containment
      ((SpanOutsideBefore) (list 'ShouldNotHappen))
      ((SpanHasBadOverlap) (list 'NotPossible))

      ((SpanContainedHere)
       (rebuild-if-needed
        (extract-span-here element span-duration)
        (lambda (replacement) (music-clone music 'element replacement))))

      ((SpanContainedStrictly) (list 'ShouldNotHappen))

      ;; If the span is completely contained after the element (or possibly if
      ;; there is no element), then it must be that the element is of null
      ;; length. The span is then in the elements (plural), so we go have a look
      ;; at them.
      ((SpanOutsideAfter)
       (let ((element-duration (if (ly:music? element) (get-total-length element) ly:moment-zero)))
         (if (not (ly:moment=? element-duration ly:moment-zero))
             (list 'NotPossible)

             (let ((elements (ly:music-property music 'elements)))
               (rebuild-if-needed
                (extract-span-here-list elements span-duration)
                (lambda (replacement) (music-clone music 'elements replacement))))))))))

(define (extract-span-here-list-acc acc elements span-duration)

  ;; FIXME: this does not work: we need to handle the case where we need to
  ;; enter the first element of the list, instead of trying to gather elements
  ;; from the list

  (if (ly:moment-is-zero? span-duration)
      (list 'Extracted (reverse acc)
            (lambda (replacement) (append replacement elements)))

      (if (null? elements)
          (list 'ShouldNotHappen)

          (let* ((first (car elements))
                 (length (get-total-length first)))
            (case (ly:compare-moments length span-duration)

              ((Greater)
               ;; The last element contains, all the rest of the length,
               ;; strictly. This only makes sense if we went through no elements
               ;; so far, or only zero-length elements.
               (let* ((acc-lengths      (map get-total-length acc))
                      (acc-total-length (fold-left ly:moment-add ly:moment-zero acc-lengths)))
                 (if (ly:moment=? acc-total-length ly:moment-zero)

                     (rebuild-if-needed
                      (extract-span-here first span-duration)
                      (lambda (replacement) (append (reverse acc) (cons replacement (cdr elements)))))

                     ;; Otherwise, it's not possible
                     (list 'NotPossible))))

              (else
               (extract-span-here-list-acc
                (cons first acc) (cdr elements) (ly:moment-sub span-duration length))))))))

(define (extract-span-here-list elements span-duration)
  (extract-span-here-list-acc '() elements span-duration))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                            ;;
;;   extract-span-from-list                                                   ;;
;;                                                                            ;;
;;   Simplified version.                                                      ;;

(define (extract-span-from-list-acc acc elements span-duration)

  ;; FIXME: this does not work: we need to handle the case where we need to
  ;; enter the first element of the list, instead of trying to gather elements
  ;; from the list

  (if (ly:moment-is-zero? span-duration)
      (cons (reverse acc) elements)

      (if (null? elements)
          '()

          (let* ((first  (car elements))
                 (length (get-total-length first)))
            (case (ly:compare-moments length span-duration)
              ((Greater) '())

              (else
               (extract-span-from-list-acc
                (cons first acc) (cdr elements) (ly:moment-sub span-duration length))))))))

(define (extract-span-from-list elements span-duration)
  (extract-span-from-list-acc '() elements span-duration))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                            ;;
;;   extract-span music start duration                                        ;;

(define (extract-span music start duration)
  (extract-span-seek
   ly:moment-zero
   music start duration))

;; ('SpanOutsideBefore ...)
;; ('SpanOutsideAfter ...)
;; ('SpanHasBadOverlap ...)
;; ('SpanContainedHere ...)
;; ('SpanContainedStrictly ...)

;; FIXME: ly:music-start ??????
