
(define (skip-as-repeat music)
  (cadr (sar #f music)))

(define (sar prev music)

  (if (music-is-of-type? music 'skip-event)
      (sar-skip prev music)

      (if (or (music-is-of-type? music 'note-event)
              (music-is-of-type? music 'event-chord))
          (list music music)

          (if (music-is-of-type? music 'simultaneous-music)
              (let* ((elements (ly:music-property music 'elements))
                     (results  (map (lambda (element) (sar prev element)) elements))
                     (prev     (car (car results)))
                     (elements (map cadr results))
                     (music    (ly:music-deep-copy music)))
                (ly:music-set-property! music 'elements elements)
                (list prev music))

              (sar-sub prev music)))))

(define (sar-skip prev skip)
  (if prev
      (let ((duration (duration-of-music skip))
            (new      (ly:music-deep-copy prev)))
        (music-or-chord-set-duration! new duration)
        (list prev new))
      (list #f skip)))

(define (sar-sub prev music)
  (let ((element (ly:music-property music 'element)))
    (if (ly:music? element)
        (let* ((result  (sar prev element))
               (prev    (car result))
               (element (cadr result))
               (music   (ly:music-deep-copy music)))
          (ly:music-set-property! music 'element element)
          (sar-subs prev music))
        (sar-subs prev music))))

(define (sar-subs prev music)
  (let* ((elements (ly:music-property music 'elements))
         (result   (sar-list prev elements))
         (prev     (car result))
         (elements (cadr result))
         (music    (ly:music-deep-copy music)))
    (ly:music-set-property! music 'elements elements)
    (list prev music)))

(define (sar-list prev musics)
  (if (null? musics)
      (list prev '())
      (let* ((music  (car musics))
             (musics (cdr musics))
             (result (sar prev music))
             (prev   (car result))
             (music  (cadr result))
             (result (sar-list prev musics))
             (prev   (car result))
             (musics (cadr result)))
        (list prev (cons music musics)))))
