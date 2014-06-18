; The hello-world for interpreters ;-)
(define (factorial n)
  (if (<= n 0) 1
    (* n (factorial (- n 1)))))

(define (main)
    (display (factorial 10)))