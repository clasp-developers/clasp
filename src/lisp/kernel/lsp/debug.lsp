(defpackage #:clasp-debug
  (:use #:cl)
  ;; low level
  (:export #:code-source-line
           #:code-source-line-pathname
           #:code-source-line-line-number
           #:code-source-line-column)
  (:export #:call-with-frame)
  (:import-from #:core #:frame)
  (:export #:frame)
  (:export #:frame-up #:frame-down)
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
           #:with-capped-stack #:cap-frame-p)
  (:export #:*frame-filters*)
  ;; mid level
  (:export #:call-with-stack)
  (:export #:up #:down)
  (:export #:map-frames #:list-frames)
  ;; misc
  (:export #:function-name-package))

(in-package #:clasp-debug)

;;; Low level interface

;;; FIXME: Unify source position stuff somehow.
;;; This one could be a core:source-pos-info, except
;;; we don't have an offset into the file.
(defstruct (code-source-line (:type vector) :named)
  pathname line-number column)

;;; FIXME: Consing up the whole list only to discard it is silly.
(defun call-with-frame (function)
  (core:call-with-backtrace
   (lambda (bt)
     (declare (core:lambda-name call-with-frame-lambda))
     (funcall function (first bt)))))

(defun frame-up (frame)
  (core:backtrace-frame-up frame))

(defun frame-down (frame)
  (core:backtrace-frame-down frame))

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

;;; Mid level interface: Navigate frames nicely

;; The frame one beyond the selected stack (i.e. exclusive limit)
(defvar *stack-top*)
;; The frame beginning the selected stack (i.e. inclusive limit)
(defvar *stack-bot*)

(defun find-bottom-frame (start)
  ;; Look for a frame truncation marker.
  ;; If none is found, use the start.
  ;; If a cap is found, don't look for a bottom beyond that.
  (do ((f start (frame-up f)))
      ((or (null f) (cap-frame-p f)) start)
    (when (truncation-frame-p f)
      ;; A truncation frame is a call to call-with-truncated-stack,
      ;; and we don't really want to keep that around, so.
      ;; If truncated-frame-p does something else later we should
      ;; maybe do something else here too.
      (return (frame-up f)))))

(defun find-top-frame (start)
  ;; Look for a frame cap.
  (do ((f start (frame-up f)))
      ((or (null f) (cap-frame-p f)) f)))

(defun call-with-stack (function &key)
  (call-with-frame
   (lambda (frame)
     (declare (core:lambda-name call-with-stack-lambda))
     (let* ((*stack-bot* (find-bottom-frame frame))
            (*stack-top* (find-top-frame *stack-bot*)))
       (funcall function *stack-bot*)))))

(defvar *frame-filters* (list 'non-lisp-frame-p))

(defun frame-visible-p (frame)
  (notany (lambda (f) (funcall f frame)) *frame-filters*))

(defun up1 (frame &optional limit)
  (do* ((prev frame next)
        (next (frame-up prev) (frame-up prev)))
       (nil)
    ;; limit is exclusive
    (when (eq next limit) (return prev))
    (when (frame-visible-p next) (return next))))
(defun down1 (frame &optional limit)
  (do* ((prev frame next)
        (next (frame-down prev) (frame-down prev)))
       (nil)
    ;; limit is inclusive
    (when (eq next limit) (return next))
    (when (frame-visible-p next) (return next))))

(defun up (frame &optional (n 1))
  (loop repeat n do (setf frame (up1 frame *stack-top*)))
  frame)

(defun down (frame &optional (n 1))
  (loop repeat n do (setf frame (down1 frame *stack-bot*)))
  frame)

;; Return the frame if it's visible, or else the next
;; visible frame up
(defun up0 (frame)
  (if (frame-visible-p frame)
      frame
      (up frame)))

(defun map-frames (function frame &key count)
  (loop for f = (up0 frame) then (up f)
        for i from 0
        when (or (and count (= i count))
                 ;; This is a bit inefficient, but interactive
                 ;; debuggers aren't usually fast path
                 (eq (up f) f))
          return (values)
        do (funcall function f)))

(defun list-frames (frame &key count)
  (let ((l nil))
    (map-frames
     (lambda (frame)
       (declare (core:lambda-name list-frames-lambda))
       (push frame l))
     frame
     :count count)
    (nreverse l)))

;;; Some filters

(defun non-lisp-frame-p (frame)
  (not (eq (frame-language frame) :lisp)))

;;; Miscellaneous.

;;; Return the package a function name conceptually belongs to.
;;; Used by top.lsp and SLDB. FIXME: Robustness
(defun function-name-package (function-name)
  (cond ((null function-name) nil) ; information is lacking
        ((stringp function-name) nil) ; C/C++ frame, inapplicable
        ((eq function-name 'cl:lambda) nil) ; anonymous
        ((symbolp function-name) (symbol-package function-name))
        ((and (consp function-name)
              (consp (cdr function-name))
              (null (cddr function-name))
              ;; Standard SETF name, or one of our FLET or LABELS names.
              (member (second function-name) '(setf flet labels)))
         (symbol-package (second function-name)))
        ((and (consp function-name)
              (eq (car function-name) 'cl:lambda))
         nil)
        ;; shrug.
        (t nil)))

;;; Called by SIGINFO handler, see gctools/interrupt.cc
(defun information-interrupt (&rest args)
  (core:safe-backtrace))
