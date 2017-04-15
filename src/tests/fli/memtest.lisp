
(in-package #:clasp-ffi.tests)

(defparameter *ptr* (clasp-ffi::%allocate-foreign-object :pointer))
(defparameter *null-ptr* (cffi:null-pointer))


(defun frgo.deref.pointer.null ( &optional (ptr *ptr*) (null-ptr *null-ptr*) )

  (unless ptr
    (setq ptr (clasp-ffi::%allocate-foreign-object :pointer)))

  (unless null-ptr
    (setq *null-ptr* (cffi:null-pointer)))

  (format *debug-io* "~% DEREF.POINTER.NULL: ptr = ~S~&" ptr)
  (format *debug-io* "~% DEREF.POINTER.NULL: null-ptr = ~S~&" null-ptr)

  (setf (cffi:mem-ref ptr :pointer) null-ptr)

  (let ((mem-ref (cffi:mem-ref ptr :pointer)))
    (format *debug-io* "~% DEREF.POINTER.NULL: mem-ref'ed ptr = ~S~&" mem-ref)
    mem-ref)
  )

(defun frgo.mem-ref-ltr ()
  (let ((result nil))
    (cffi:with-foreign-object (p :char)
      (setf (cffi::mem-ref p :char) 42)
      (cffi:mem-ref (progn (push 1 result) p) :char (progn (push 2 result) 0))
      (nreverse result))))
