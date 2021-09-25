(define (add-trailing-silence music duration)
  (let* ((position (length-of-music music))
         (duration (ly:moment-sub duration position))
         (result (ats music position duration)))
    (case (car result)
      ((OK) (cadr result))
      (else (ly:error "add-trailing-silence: impossible to add trailing silence to this")))))

(define (ats music position duration)

  ;; If music is simultaneous, we extend all the sub-elements. If it indeed
  ;; succeeded, then we rebuild the music with the extended elements. Otherwise,
  ;; we fail.
  (if (music-is-of-type? music 'simultaneous-music)
      (let* ((elements (ly:music-property music 'elements))
             (results (map (lambda (element) (ats element position duration)) elements)))
        (if (for-all (lambda (result) (eq? (car result) 'OK)) results)
            (list 'OK (music-clone music 'elements (map cadr results)))
            (list 'Error)))

      ;; Otherwise, we check if the music is the last element (or contains it)
      ;; and, in that case, we continue to the sub-elements.
      (let ((length (length-of-music music)))
        (if (or (ly:moment<=? position ly:moment-zero) (ly:moment<>? length position))
            (list 'Error)
            (ats-sub music position duration)))))

(define (ats-sub music position duration)
  (let ((element (ly:music-property music 'element)))
    (if (ly:music? element)
        (let ((result (ats element position duration)))
          (case (car result)
            ((OK) (list 'OK (music-clone music 'element (cadr result))))
            (else (ats-subs music (ly:moment-sub position (length-of-music element)) duration))))
        (ats-subs music position duration))))

(define (ats-subs music position duration)
  (let* ((elements (ly:music-property music 'elements))
         (result   (ats-list '() elements position duration)))
    (case (car result)
      ((OK) (list 'OK (music-clone music 'elements (cadr result))))
      (else (list 'Error)))))

(define (ats-list acc musics position duration)

  (if (null? musics)
      (list 'Error)

      (let* ((music (car musics))
             (musics (cdr musics))
             (length (length-of-music music)))

        (if (ly:moment<? length position)
            (ats-list (cons music acc) musics (ly:moment-sub position length) duration)

            (let ((result (ats music position duration)))
              (case (car result)
                ((OK) (list 'OK (reverse-append acc (cons (cadr result) musics))))

                (else
                 (let ((silence (make-music 'SkipEvent 'duration (make-duration-of-length duration))))
                   (list 'OK (reverse-append acc (cons music (cons silence musics))))))))))))
