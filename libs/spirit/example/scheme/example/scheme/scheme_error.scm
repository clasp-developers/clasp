(define what blah) ; blah not found

(define
  (foo x)
    (+ x 456))

(define
  (bar x)
    (+ x y)) ; y not found

(define (f1) (foo 123))
(define (f2) (foo z)) ; z not found

(define foo 123) ; redefinition

(define (f3) (foo 123 456)) ; incorrect arity

(define (f4) (bar 999)) ; bar should not be found

(define (main) ) ; no body