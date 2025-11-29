;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                            ;;
;;   get-partial-length music                                                 ;;
;;                                                                            ;;
;;   Crawl through music, looking for partial elements. Return the length     ;;
;;   of the first one found, or a zero length if there is none.               ;;

(define (get-partial-length music)
  (let ((result (get-partial music)))
    (case (car result)
      ((Partial) (ly:duration->moment (ly:music-property (cadr result) 'duration)))
      ((NoPartial) ly:moment-zero))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                            ;;
;;   get-partial music                                                        ;;
;;                                                                            ;;
;;   Auxiliary function that crawls through music, looking for partial        ;;
;;   elements. Returns a list whose first element is either 'Partial or       ;;
;;   'NoPartial. In case of 'Partial, the list has a second element, which    ;;
;;   is the partial object that has been found.                               ;;

(define (get-partial music)
  (if (music-is-of-type? music 'partial-set)
      (list 'Partial music)
      (let* ((element (ly:music-property music 'element))
             (result  (if (ly:music? element)
                          (get-partial element)
                          (list 'NoPartial))))
        (case (car result)
          ((Partial) result)
          ((NoPartial)
           (let ((elements (ly:music-property music 'elements)))
             (get-partial-list elements)))))))

(define (get-partial-list musics)
  (if (null? musics)
      (list 'NoPartial)
      (let* ((music  (car musics))
             (musics (cdr musics))
             (result (get-partial music)))
        (case (car result)
          ((Partial) result)
          ((NoPartial) (get-partial-list musics))))))
