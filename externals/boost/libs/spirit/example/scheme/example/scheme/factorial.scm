; The hello-world for interpreters ;-)
(define (factorial n)
  (if (<= n 0) 1
    (* n (factorial (- n 1)))))
