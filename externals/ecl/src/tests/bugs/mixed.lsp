;-*- Mode:     Lisp -*-
;;;; Contains: Some regression tests for ECL

(in-package :cl-test)


;;; (EXT:PACKAGE-LOCK) returned the wrong value.
;;; Fixed in 77a267c7e42860affac8eddfcddb8e81fccd44e5

(deftest mixed-0001-package-lock
  (progn
    ;; Don't know the first state
    (ext:package-lock "CL-USER" nil)
    (assert (eq nil
                (ext:package-lock "CL-USER" t)))
    (assert (eq t
                (ext:package-lock "CL-USER" nil)))
    (assert (eq nil
                (ext:package-lock "CL-USER" nil)))))
