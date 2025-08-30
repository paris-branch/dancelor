(define (fold-left f x l)
  (if (pair? l) (fold-left f (f x (car l)) (cdr l)) x))

(define (reverse-append l1 l2)
  (if (null? l1) l2
      (reverse-append (cdr l1) (cons (car l1) l2))))

(define (reverse lst)
  (reverse-append lst '()))

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

(define (forall p l)
  (if (null? l)
      #t
      (and (p (car l)) (forall p (cdr l)))))

(define (for-all p l) (forall p l))

(define (exists p l)
  (if (null? l)
      #f
      (or (p (car l)) (exists p (cdr l)))))
