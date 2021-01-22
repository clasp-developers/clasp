(in-package #:clasp-tests)

(defun qsort (base nmemb size fun-compar)
   (let ((basenew base)
         (nmembnew nmemb)
         (sizenew size)
         (funcomparnew fun-compar))
     (clasp-ffi:%foreign-funcall "qsort" :pointer basenew :int nmembnew :int sizenew :pointer funcomparnew :void)))
   
(clasp-ffi:%defcallback (< :convention :cdecl)
                        :int
                        (a b)
                        (:pointer :pointer)
                        (let ((a a)
                              (b b))
                          (block <
                            (let ((x (clasp-ffi:%mem-ref a :int 0))
                                  (y (clasp-ffi:%mem-ref b :int 0)))
                              (cond ((> x y) 1) ((< x y) -1) (t 0))))))

(test cffi-defcallback
      (progn
        (format t "Native defcallback starting~%")
        (let* ((sorted (let* ((array (clasp-ffi:%foreign-alloc (* 10 (clasp-ffi:%foreign-type-size :int)))))
                         (unwind-protect
                              (progn
                                ;; initialize array.
                                (loop for i from 0 and n in '(7 2 10 4 3 5 1 6 9 8)
                                      do (clasp-ffi:%mem-set array :int n (* i (clasp-ffi:%foreign-type-size :int))))
                                ;; sort it.
                                (qsort array 10 (clasp-ffi:%foreign-type-size :int) (clasp-ffi:%get-callback '<))
                                ;; return it as a list.
                                (loop for i from 0 below 10
                                      collect (clasp-ffi:%mem-ref array :int (* i (clasp-ffi:%foreign-type-size :int)))))
                           (clasp-ffi:%foreign-free array))))
               (result (equalp '(1 2 3 4 5 6 7 8 9 10) sorted)))
          (format t "Native defcallback sorted -> ~a~%" sorted)
          (format t "Native defcallback result -> ~a~%" result)
          result)))


