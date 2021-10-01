(in-package #:clasp-tests)

(test CERROR.6
      (locally (declare (optimize (safety 3)))
        (HANDLER-BIND ((SIMPLE-ERROR #'continue))
          (PROGN (CERROR "Wooo" 'SIMPLE-ERROR) 10)))
      (10))
