(progn
  (format t "Outer returns: ~a~%"
          (multiple-value-call #'list
            (catch 'a
              (format t "Inner returns: ~a~%"
                      (multiple-value-call #'list
                        (catch 'b
                          (unwind-protect
                               (throw 'b (values 1 2 3))
                            (throw 'a (values 10 20 30)))
                          )))))))
                        
                               




(progn
  (format t "Outer returns: ~a~%"
          (multiple-value-call #'list
            (catch 'a
              (format t "Inner returns: ~a~%"
                      (multiple-value-call #'list
                        (catch 'b
                          (unwind-protect
                               (throw 'b (values 1 2 3))
                            (values 10 20 30))
                          )))))))


(progn (format t "Outer returns: ~a~%" (multiple-value-call #'list (catch 'a (format t "Inner returns: ~a~%" (multiple-value-call #'list (catch 'b (unwind-protect (throw 'b (values 1 2 3)) (values 10 20 30)))))))))




(catch 'b
  (unwind-protect
       (throw 'b (values 1 2 3))
    (values 10 20 30)))

(catch 'b (unwind-protect (throw 'b (values 1 2 3)) (values 10 20 30))) 
