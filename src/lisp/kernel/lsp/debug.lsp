(defpackage #:clasp-debug
  (:use #:cl)
  ;; low level
  (:export #:code-source-line
           #:code-source-line-pathname
           #:code-source-line-line-number
           #:code-source-line-column)
  (:export #:frame-function #:frame-arguments
           #:frame-locals #:frame-source-position
           #:frame-language)
  (:export #:frame-function-name
           #:frame-function-lambda-list
           #:frame-function-source-position
           #:frame-function-form
           #:frame-function-documentation)
  (:export #:disassemble-frame)
  ;; frame selection
  (:export #:with-truncated-stack #:truncation-frame-p
           #:with-capped-stack #:cap-frame-p))

(in-package #:clasp-debug)

;;; Low level interface

;;; FIXME: Unify source position stuff somehow.
;;; This one could be a core:source-pos-info, except
;;; we don't have an offset into the file.
(defstruct (code-source-line (:type vector) :named)
  pathname line-number column)

(defun frame-function (frame)
  (core:backtrace-frame-closure frame))

(defun frame-arguments (frame)
  (coerce (core:backtrace-frame-arguments frame) 'list))

(defun frame-locals (frame)
  ;; TODO
  (declare (ignore frame))
  nil)

(defun frame-source-position (frame)
  (multiple-value-bind (pathname line-number column)
      (core::code-source-position
       (core:backtrace-frame-return-address frame))
    (make-code-source-line
     :pathname pathname
     :line-number line-number
     :column column)))

(defun frame-language (frame)
  (core:backtrace-frame-type frame))

(defun frame-function-name (frame)
  (core:backtrace-frame-print-name frame))

(defun frame-function-lambda-list (frame)
  ;; FIXME: Get from function description
  (let ((f (frame-function frame)))
    (if f
        (values (ext:function-lambda-list f) t)
        (values nil nil))))

(defun frame-function-source-position (frame)
  ;; TODO: Get from function description
  (declare (ignore frame))
  nil)

(defun frame-function-form (frame)
  ;; TODO
  (declare (ignore frame))
  nil)

(defun frame-function-documentation (frame)
  (let ((f (frame-function frame)))
    (if f
        (documentation f 'function)
        nil)))

(defun disassemble-frame (frame)
  ;; TODO: Use function-start-address, function-end-address
  (let ((f (frame-function frame)))
    (when f (disassemble f))))

;;; Frame selection

;;; This is not the most inspired way to do it, but should work.
;;; More efficient would be to keep frame pointers around and compare
;;; them. I think the current code does this but only kinda?

(declaim (notinline call-with-truncated-stack))
(defun call-with-truncated-stack (function) (funcall function))

(defmacro with-truncated-stack ((&key) &body body)
  ;; progn to avoid declarations
  `(call-with-truncated-stack (lambda () (progn ,@body))))

(defun truncation-frame-p (frame)
  (eq (frame-function-name frame) 'call-with-truncated-stack))

(declaim (notinline call-with-capped-stack))
(defun call-with-capped-stack (function) (funcall function))

(defmacro with-capped-stack ((&key) &body body)
  `(call-with-capped-stack (lambda () (progn ,@body))))

(defun cap-frame-p (frame)
  (eq (frame-function-name frame) 'call-with-capped-stack))

;;; Miscellaneous. Called by SIGINFO handler, see gctools/interrupt.cc

(defun information-interrupt (&rest args)
  (core:safe-backtrace))
