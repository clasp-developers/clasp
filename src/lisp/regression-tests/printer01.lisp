(in-package #:clasp-tests)

(test print-1
      (string=
       "-1"
       (LET ((*PRINT-BASE* 16))
         (WITH-OUTPUT-TO-STRING (*STANDARD-OUTPUT*) (write -1)))))




  


