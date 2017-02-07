(in-package #:clasp-ffi.tests)

;;;# Calling with Built-In C Types
;;;
;;; Tests calling standard C library functions both passing and
;;; returning each built-in type.

;;; Don't run these tests if the implementation does not support
;;; foreign-funcall.
#-cffi-sys::no-foreign-funcall
(progn

  (deftest funcall.char
      (cffi:foreign-funcall "toupper" :char (char-code #\a) :char)
    #.(char-code #\A))

  (deftest funcall.int.1
      (cffi:foreign-funcall "abs" :int -100 :int)
    100)

  (defun funcall-abs (n)
    (cffi:foreign-funcall "abs" :int n :int))

;;; regression test: lispworks's %foreign-funcall based on creating
;;; and caching foreign-funcallables at macro-expansion time.
  (deftest funcall.int.2
      (funcall-abs -42)
    42)

  (deftest funcall.long
      (cffi:foreign-funcall "labs" :long -131072 :long)
    131072)

  #-cffi-sys::no-long-long
  (deftest funcall.unsigned-long-long
      (let ((ullong-max (1- (expt 2 (* 8 (cffi:foreign-type-size :unsigned-long-long))))))
        (eql ullong-max
             (cffi:foreign-funcall "ullong" :unsigned-long-long ullong-max
                                   :unsigned-long-long)))
    t)

  (deftest funcall.float
      (cffi:foreign-funcall "my_sqrtf" :float 16.0 :float)
    4.0)

  (deftest funcall.double
      (cffi:foreign-funcall "sqrt" :double 36.0d0 :double)
    6.0d0)

  #+(and scl long-float)
  (deftest funcall.long-double
      (cffi:foreign-funcall "sqrtl" :long-double 36.0l0 :long-double)
    6.0l0)

  (deftest funcall.string.1
      (cffi:foreign-funcall "strlen" :string "Hello" :int)
    5)

  (deftest funcall.string.2
      (cffi:with-foreign-pointer-as-string (s 100)
        (setf (cffi:mem-ref s :char) 0)
        (cffi:foreign-funcall "strcpy" :pointer s :string "Hello" :pointer)
        (cffi:foreign-funcall "strcat" :pointer s :string ", world!" :pointer))
    "Hello, world!")

  (deftest funcall.string.3
      (cffi:with-foreign-pointer (ptr 100)
        (cffi:lisp-string-to-foreign "Hello, " ptr 8)
        (cffi:foreign-funcall "strcat" :pointer ptr :string "world!" :string))
    "Hello, world!")

;;;# Calling Varargs Functions

  ;; The CHAR argument must be passed as :INT because chars are promoted
  ;; to ints when passed as variable arguments.
  (deftest funcall.varargs.char
      (cffi:with-foreign-pointer-as-string (s 100)
        (setf (cffi:mem-ref s :char) 0)
        (cffi:foreign-funcall "sprintf" :pointer s :string "%c" :int 65 :int))
    "A")

  (deftest funcall.varargs.int
      (cffi:with-foreign-pointer-as-string (s 100)
        (setf (cffi:mem-ref s :char) 0)
        (cffi:foreign-funcall "sprintf" :pointer s :string "%d" :int 1000 :int))
    "1000")

  (deftest funcall.varargs.long
      (cffi:with-foreign-pointer-as-string (s 100)
        (setf (cffi:mem-ref s :char) 0)
        (cffi:foreign-funcall "sprintf" :pointer s :string "%ld" :long 131072 :int))
    "131072")

;;; There is no FUNCALL.VARARGS.FLOAT as floats are promoted to double
;;; when passed as variable arguments.  Currently this fails in SBCL
;;; and CMU CL on Darwin/ppc.
  (deftest funcall.varargs.double
      (cffi:with-foreign-pointer-as-string (s 100)
        (setf (cffi:mem-ref s :char) 0)
        (cffi:foreign-funcall "sprintf" :pointer s :string "%.2f"
                              :double (coerce pi 'double-float) :int))
    "3.14")

  #+(and scl long-float)
  (deftest funcall.varargs.long-double
      (with-foreign-pointer-as-string (s 100)
        (setf (mem-ref s :char) 0)
        (cffi:foreign-funcall "sprintf" :pointer s :string "%.2Lf"
                              :long-double pi :int))
    "3.14")

  (deftest funcall.varargs.string
      (cffi:with-foreign-pointer-as-string (s 100)
        (setf (cffi:mem-ref s :char) 0)
        (cffi:foreign-funcall "sprintf" :pointer s :string "%s, %s!"
                              :string "Hello" :string "world" :int))
    "Hello, world!")

;;; See DEFCFUN.DOUBLE26.
  (deftest funcall.double26
      (cffi:foreign-funcall "sum_double26"
                            :double 3.14d0 :double 3.14d0 :double 3.14d0
                            :double 3.14d0 :double 3.14d0 :double 3.14d0
                            :double 3.14d0 :double 3.14d0 :double 3.14d0
                            :double 3.14d0 :double 3.14d0 :double 3.14d0
                            :double 3.14d0 :double 3.14d0 :double 3.14d0
                            :double 3.14d0 :double 3.14d0 :double 3.14d0
                            :double 3.14d0 :double 3.14d0 :double 3.14d0
                            :double 3.14d0 :double 3.14d0 :double 3.14d0
                            :double 3.14d0 :double 3.14d0 :double)
    81.64d0)

;;; See DEFCFUN.FLOAT26.
  (deftest funcall.float26
      (cffi:foreign-funcall "sum_float26"
                            :float 5.0 :float 5.0 :float 5.0 :float 5.0 :float 5.0
                            :float 5.0 :float 5.0 :float 5.0 :float 5.0 :float 5.0
                            :float 5.0 :float 5.0 :float 5.0 :float 5.0 :float 5.0
                            :float 5.0 :float 5.0 :float 5.0 :float 5.0 :float 5.0
                            :float 5.0 :float 5.0 :float 5.0 :float 5.0 :float 5.0
                            :float 5.0 :float)
    130.0)

;;; Funcalling a pointer.
  (deftest funcall.f-s-p.1
      (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "abs") nil :int -42 :int)
    42)

;;; RT: CLISP returns NIL instead of a null-pointer

  (deftest funcall.pointer-not-nil
      (not (null (cffi:foreign-funcall "strchr" :string "" :int 1 :pointer)))
    t)

  ) ;; #-cffi-sys::no-foreign-funcall
