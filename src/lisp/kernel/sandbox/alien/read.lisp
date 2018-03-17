(in-package #:clasp-sandbox)

;; even in this macro you still need to use the special read function sicl-reader:read.
;; this macro is just for avoiding recomputing things too much.
(defmacro with-sandbox-read (environment &body body)
  `(let ((sicl-reader:*readtable* (sicl-reader:copy-readtable nil))
         (sicl-reader:*environment* ,environment))
     (sicl-reader:set-dispatch-macro-character
      #\# #\.
      (lambda (stream char parameter)
        (declare (ignore char parameter))
        (eval-with-genv (sandbox-read stream sicl-reader:*environment* t nil t))))
     ,@body))
