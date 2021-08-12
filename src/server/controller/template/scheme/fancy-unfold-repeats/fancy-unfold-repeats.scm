
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
