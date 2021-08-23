(define (fold-left f x l)
  (if (pair? l) (fold-left f (f x (car l)) (cdr l)) x))

(define (reverse-aux lst acc)
  (if (null? lst) acc
      (reverse-aux (cdr lst) (cons (car lst) acc))))

(define (reverse lst)
  (reverse-aux lst '()))

(define (pair a b) (cons a b))

(define (id x) x)

(define (assert-singleton x)
  (if (and (list? x) (not (null? x)) (null? (cdr x)))
      (car x)
      (ly:error "assert-singleton: failed on: ~a" x)))

(define (displayl . l)
  (map display l))

(define (flatten l)
  (apply append l))
