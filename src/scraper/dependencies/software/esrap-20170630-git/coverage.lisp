;;;; coverage.lisp --- Helper script for coverage report generation.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(require :sb-cover)

(defun compute-coverage-for-system (system
                                    &key
                                      (output-directory
                                       (merge-pathnames
                                        (concatenate 'string (string system) "/")
                                        "coverage-report/")))
  (flet ((set-store-coverage (storep)
           (eval `(declaim (optimize (sb-cover:store-coverage-data ,(if storep 3 0))))))
         (load-system-silently (system &rest args)
           (let* ((*standard-output* (make-broadcast-stream))
                  (*trace-output*    *standard-output*))
             (handler-bind ((style-warning #'muffle-warning))
               (apply #'asdf:load-system system args)))))
    (load-system-silently system) ; load dependencies
    (unwind-protect
         (progn
           (set-store-coverage t)
           (load-system-silently system :force t)
           (set-store-coverage nil)
           (let ((*compile-print*    nil)
                 (*compile-progress* nil)
                 (*compile-verbose*  nil))
             (asdf:test-system system))
           (sb-cover:report output-directory))
      (set-store-coverage nil)
      (load-system-silently system :force t)
      (sb-cover:clear-coverage))))

(mapcar #'compute-coverage-for-system '(:esrap))
