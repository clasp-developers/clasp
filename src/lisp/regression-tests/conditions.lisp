(in-package #:clasp-tests)

(test CERROR.6
      (= 10
         (locally (declare (optimize (safety 3)))
           (HANDLER-BIND ((SIMPLE-ERROR #'(LAMBDA (C) (CONTINUE C))))
             (PROGN (CERROR "Wooo" 'SIMPLE-ERROR) 10)))))
