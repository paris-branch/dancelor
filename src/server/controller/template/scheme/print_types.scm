(define (print-indent indent)
  (if (> indent 0)
      (begin
        (display " ")
        (print-indent (- indent 1)))))

(define (print-types-aux indent music)
  (display (ly:music-property music 'types))
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
   (ly:music-property music 'elements))

  (map
   (lambda (element)
     (print-indent indent)
     (display "~ ")
     (print-types-aux (+ indent 2) element))
   (ly:music-property music 'articulations)))

(define (print-types music)
  (print-types-aux 0 music)
  music)
