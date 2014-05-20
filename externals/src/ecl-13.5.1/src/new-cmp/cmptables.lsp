;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DISPATCH TABLES FOR FRONT-ENDS AND BACK-ENDS
;;;

;;; ----------------------------------------------------------------------
;;; CONSTRUCTORS
;;;

(in-package "C-DATA")

(defun trace-function (f)
  #'(lambda (&rest args)
      (terpri) (princ #\>) (princ f)
      (loop with *print-length* = 4 with *print-depth* = 3
         for i in args do (print i))
      (multiple-value-prog1 (apply f args)
        (terpri) (princ #\<) (princ f))))

(defun make-dispatch-table (pairs)
  (loop with output = (make-hash-table :size (* 2 (length pairs)) :test #'eq)
     for (name . function) in pairs
     do (setf (gethash name output) function)
     finally (return output))
  #+nil
  (loop with output = (make-hash-table :size (* 2 (length pairs)) :test #'eq)
     for (name . function) in pairs
     for traced-function = (trace-function function)
     do (setf (gethash name output) traced-function)
     finally (return output)))

(defun extend-dispatch-table (pairs table)
  (loop with output = (make-dispatch-table pairs)
     for k being the hash-key in output using (hash-value v)
     do (setf (gethash output k) v)
     finally (return output)))

;;; ------------------------------------------------------------------
;;; COMMON LISP FORMS TRANSLATORS
;;;

(in-package "C")

(defconstant +c1-dispatch-data+
 '(
   ;; cmpblock.lsp
   (block . c1block)
   (return-from . c1return-from)

   ;; cmpcall.lsp
   (funcall . c1funcall)
   
   ;; cmpcatch.lsp
   (catch . c1catch)
   (throw . c1throw)
   (unwind-protect . c1unwind-protect)

   ;; cmpcbk.lsp
   ;; (ffi:defcallback . c1-defcallback)

   ;; cmpeval
   (progn . c1progn)

   ;; cmpffi.lsp
   (ffi:clines . c1clines)
   (ffi:c-inline . c1c-inline)

   ;; cmpflet
   (flet . c1flet)
   (labels . c1labels)
   (do-flet/labels . c1do-flet/labels)
   (make-flet/labels-closure . c1make-flet/labels-closure)
   (locally . c1locally)
   (macrolet . c1macrolet)
   (symbol-macrolet . c1symbol-macrolet)

   ;; cmpfun.lsp
   (apply . c1apply)

   ;; cmpif.lsp
   (if . c1if)

   ;; cmplet.lsp
   (let . c1let)
   (let* . c1let*)

   ;; cmpmulti.lsp
   (multiple-value-call . c1multiple-value-call)
   (multiple-value-prog1 . c1multiple-value-prog1)
   (values . c1values)
   (multiple-value-setq . c1multiple-value-setq)
   (multiple-value-bind . c1multiple-value-bind)

   ;; cmpspecial.lsp
   (quote . c1quote)
   (function . c1function)
   (the . c1the)
   (eval-when . c1eval-when)
   (ext:with-backend . c1with-backend)
   (declare . c1declare)
   (ext:compiler-let . c1compiler-let)

   ;; cmpstack.lsp
   (with-stack . c1with-stack)
   (stack-push . c1stack-push)
   (stack-push-values . c1stack-push-values)
   (stack-pop . c1stack-pop)
   (si::apply-from-stack-frame . c1apply-from-stack-frame)

   ;; cmpstructures.lsp
   ;; (sys::structure-ref . c1structure-ref)
   ;; (sys::structure-set . c1structure-set)

   ;; cmptag.lsp
   (tagbody . c1tagbody)
   (go . c1go)

   ;; cmptop.lsp
   (load-time-value . c1load-time-value)

   ;; cmptranslate.lsp
   (values-ref . c1values-ref)

   ;; cmpvar.lsp
   (setq . c1setq)
   (psetq . c1psetq)
   (progv . c1progv)
   ))

(defparameter +c1-dispatch-table+ (make-dispatch-table +c1-dispatch-data+))
