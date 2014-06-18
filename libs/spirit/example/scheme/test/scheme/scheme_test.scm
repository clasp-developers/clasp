; These tests demostrate the functionality of the scheme
; compiler/interpreter

(define (dbl x) (+ x x))

(define len 123)

(define (test1)
    (= (dbl len) 246))

; The hello-world for interpreters ;-)
(define (factorial n)
  (if (<= n 0) 1
    (* n (factorial (- n 1)))))

(define (test2)
    (= (factorial 10) 3628800))

; Fibonacci using lambda
(define fib
  (lambda (n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2))))))

(define (test3) (= (fib 10) 55))

; nested functions
(define (foo x)
    (define (bar y z) (list x y z))
    (bar 9 (+ x 2)))

(define (test4)
    (= (foo 100) (quote ( 100 9 102 ))))

