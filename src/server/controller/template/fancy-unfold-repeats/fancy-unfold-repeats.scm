(define (print-indent indent)
  (if (> indent 0)
      (begin
        (display " ")
        (print-indent (- indent 1)))))

(define (print-types-aux indent music)
  (display (ly:music-property music 'types))

  (let ((total-duration (ly:music-property music 'niols-total-duration)))
    (display " ")(display (ly:duration-length total-duration)))
  (let ((element (ly:music-property music 'element))
        (elements (ly:music-property music 'elements)))
    (if (ly:music? element) (display " element"))
    (if (not (null? elements)) (display " elements")))
  (newline)

  (let ((element (ly:music-property music 'element)))
    (if (ly:music? element)
        (begin
          (print-indent indent)
          (display "+ ")
          (print-types-aux (+ indent 2) element))))

  (map
   (lambda (element)
     (print-indent indent)
     (display "- ")
     (print-types-aux (+ indent 2) element))
   (ly:music-property music 'elements)))

(define (print-types music) (print-types-aux 0 music))







(define (extract-next-group groups-and-musics duration)
  (let* ((groups (car groups-and-musics))
         (musics (cdr groups-and-musics))
         (result (extract-span-from-list musics duration))
         (group  (car result))
         (musics (cdr result)))
    (cons (cons group groups) musics)))

(define (unfold-absent-volta-repeat music times start durations)

  (let* ((duration (fold-left ly:moment-add ly:moment-zero durations))
         (result   (extract-span music start duration)))

    (case (car result)

      ((Extracted)
       (let* ((elements (cadr result))
              (builder  (caddr result))
              (groups   (fold-left extract-next-group (cons '() elements) durations))
              (groups   (reverse (car groups)))
              (main     (make-sequential-music (car groups)))
              (alts     (map make-sequential-music (cdr groups)))
              (unfolded (make-repeat "unfold" times main alts)))
         (builder (list unfolded))))

      (else '()))))

;; (define (fancy-unfold-repeats-one music)
;;   (let ((first-volta (unfold-first-volta-repeat ly:moment-zero music)))
;;     (case (cadr first-volta)
;;       ((FoundVolta)
;;        (let* ((music     (car first-volta))
;;               (start     (caddr first-volta))
;;               (times     (cadddr first-volta)) ;; number of repetitions of the volta
;;               (durations (cddddr first-volta)))
;;          (list 'Unfolded (unfold-absent-volta-repeat music times start durations))))
;;       (else (list 'NoVolta)))))

;; (define (fancy-unfold-repeats music)
;;   (add-total-durations music)
;;   (let ((result (fancy-unfold-repeats-one music)))
;;     (case (car result)
;;       ((Unfolded) (fancy-unfold-repeats (cadr result)))
;;       ((NoVolta) music))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

(define (fancy-unfold-repeats-here-once main others)
  (let ((main-result (unfold-first-volta-repeat ly:moment-zero main)))
    (case (cadr main-result)
      ((FoundVolta)
       (let* ((main      (car main-result))
              (start     (caddr main-result))
              (times     (cadddr main-result)) ;; number of repetitions of the volta
              (durations (cddddr main-result)))

         (append
          (list 'Unfolded main)
          (map (lambda (other)
                 (unfold-absent-volta-repeat other times start durations))
               others))))

      (else (list 'NothingToDo)))))

(define (fancy-unfold-repeats-here main others)
  (add-total-durations main)
  (map add-total-durations others)
  (let ((result (fancy-unfold-repeats-here-once main others)))
    (case (car result)
      ((Unfolded) (fancy-unfold-repeats-here (cadr result) (cddr result)))
      ((NothingToDo) (cons main others)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

(define (fancy-unfold-repeats-sub music)
  (let ((sub (ly:music-property music 'element)))
    (if (ly:music? sub)
        (let ((sub (fancy-unfold-repeats sub)))
          (music-clone music 'element sub))
        music)))

(define (fancy-unfold-repeats-subs music)
  (let* ((subs (ly:music-property music 'elements))
         (subs (map fancy-unfold-repeats subs)))
    (music-clone music 'elements subs)))

(define (fancy-unfold-repeats music)
  (if (music-is-of-type? music 'simultaneous-music)
      (let* ((elements (ly:music-property music 'elements))
             (main     (car elements))
             (others   (cdr elements))
             (elements (fancy-unfold-repeats-here main others)))
        (music-clone music 'elements elements))
      (let* ((music (fancy-unfold-repeats-sub music))
             (music (fancy-unfold-repeats-subs music)))
        music)))
