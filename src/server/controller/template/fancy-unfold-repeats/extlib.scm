(define (fold-left f x l)
  (if (pair? l) (fold-left f (f x (car l)) (cdr l)) x))

(define (reverse-aux lst acc)
  (if (null? lst) acc
      (reverse-aux (cdr lst) (cons (car lst) acc))))

(define (reverse lst)
  (reverse-aux lst '()))

(define (pair a b) (cons a b))

(define (id x) x)
