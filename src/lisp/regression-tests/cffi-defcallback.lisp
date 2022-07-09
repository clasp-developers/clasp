
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*package* (find-package :cl-user)))
    (load "sys:quicklisp;setup.lisp")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cffi))


(cffi:defcfun "qsort" :void
  (base :pointer)
  (nmemb :int)
  (size :int)
  (fun-compar :pointer))
   
(cffi:defcallback < :int ((a :pointer) (b :pointer))
  (let ((x (cffi:mem-ref a :int))
        (y (cffi:mem-ref b :int)))
    (cond ((> x y) 1)
          ((< x y) -1)
          (t 0))))


(test cffi-defcallback
      (progn
        (format t "cffi-defcallback starting~%")
        (let* ((sorted (cffi:with-foreign-object (array :int 10)
                         ;; Initialize array.
                         (loop for i from 0 and n in '(7 2 10 4 3 5 1 6 9 8)
                               do (setf (cffi:mem-aref array :int i) n))
                         ;; Sort it.
                         (qsort array 10 (cffi:foreign-type-size :int) (cffi:callback <))
                         ;; Return it as a list.
                         (loop for i from 0 below 10
                               collect (cffi:mem-aref array :int i))))
               (result (equalp '(1 2 3 4 5 6 7 8 9 10) sorted)))
          (format t "cffi-defcallback sorted -> ~a~%" sorted)
          (format t "cffi-defcallback result -> ~a~%" result)
          result)))


