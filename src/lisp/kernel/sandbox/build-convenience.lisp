(in-package #:clasp-sandbox)

(defun make-compilation-environment ()
  (let* ((e (make-instance 'evaluation-environment))
         (c (make-instance 'compilation-environment:compilation-environment
                           :evaluation-environment e)))
    (setf (compilation-environment e) c)
    (initialize-compiler-environment c)
    c))
