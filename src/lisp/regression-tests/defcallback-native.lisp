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
      (let* ((intsize (clasp-ffi:%foreign-type-size :int))
             (array
               (clasp-ffi:%foreign-alloc (* 10 intsize))))
        (unwind-protect
             (progn
               ;; initialize array.
               (loop for i from 0 and n in '(7 2 10 4 3 5 1 6 9 8)
                     do (clasp-ffi:%mem-set array :int n (* i intsize)))
               ;; sort it.
               (qsort array 10 intsize (clasp-ffi:%get-callback '<))
               ;; return it as a list.
               (loop for i from 0 below 10
                     collect (clasp-ffi:%mem-ref array :int (* i intsize))))
          (clasp-ffi:%foreign-free array)))
      ((1 2 3 4 5 6 7 8 9 10)))

;;; fcntl(int fd, int cmd, ...) has a variadic third argument. Calling it through
;;; a non-variadic function type is undefined behaviour that happens to work on
;;; x86-64 SysV, where variadic arguments use the same registers as fixed ones,
;;; and loses the argument on Darwin arm64, where they are passed on the stack:
;;; the callee reads an unwritten slot and sees zero. shm_open's mode is the
;;; usual casualty. F_GETFD=1, F_SETFD=2 and FD_CLOEXEC=1 on both Linux and macOS.
(test-true foreign-funcall-varargs-passes-variadic-argument
           (multiple-value-bind (r w) (core:pipe)
             (declare (ignore w))
             (clasp-ffi:%foreign-funcall-varargs "fcntl" 2 :int r :int 2 :int 1 :int)
             (eql 1 (clasp-ffi:%foreign-funcall "fcntl" :int r :int 1 :int))))

(test-true foreign-funcall-pointer-varargs-passes-variadic-argument
           (multiple-value-bind (r w) (core:pipe)
             (declare (ignore w))
             (let ((fcntl (core:dlsym :rtld-default "fcntl")))
               (clasp-ffi:%foreign-funcall-pointer-varargs fcntl 2 :int r :int 2 :int 1 :int)
               (eql 1 (clasp-ffi:%foreign-funcall "fcntl" :int r :int 1 :int)))))

;;; A call with no variadic arguments must be unaffected: it still builds a
;;; non-variadic function type and must succeed rather than return -1.
(test-true foreign-funcall-fixed-args-unaffected
           (multiple-value-bind (r w) (core:pipe)
             (declare (ignore w))
             (>= (clasp-ffi:%foreign-funcall "fcntl" :int r :int 1 :int) 0)))
