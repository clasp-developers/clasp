;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2006, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPSTACK Manipulation of the lisp stack from C code
;;;;
;;;; Following special forms are provided:
;;;;
;;;;	(WITH-STACK {form}*)
;;;;		Executes given forms, restoring the lisp stack on output.
;;;;	(STACK-PUSH form)
;;;;	(STACK-PUSH-VALUES form)
;;;;	(STACK-POP nvalues)
;;;;

(in-package "COMPILER")

(defun c1with-stack (destination forms)
  (let* ((var-name (pop forms))
         (var (make-var :name var-name :kind :object :type t))
         (cleanup (c1stack-frame-close var))
         (*cmp-env* (cmp-env-register-cleanup
                     cleanup
                     (cmp-env-register-var var (cmp-env-copy *cmp-env*)))))
    (nconc (c1bind (list var))
           (c1stack-frame-open var)
           (c1translate destination `(progn ,@forms))
           (c1stack-frame-close var)
           (c1unbind (list var)))))

(defun c1stack-push (destination args)
  (let* ((var (c1vref (first args)))
         (value (second args)))
    (nconc (c1translate 'VALUE0 value)
           (c1stack-frame-push var 'VALUE0))))

(defun c1stack-push-values (destination args)
  (unless (eq destination 'TRASH)
    (error "In C1STACK-PUSH-VALUES, destination should be TRASH"))
  (let* ((frame-var (pop args))
         (form (pop args)))
    (nconc (c1translate 'VALUES form)
           (c1stack-frame-push-values (c1vref frame-var)))))

(defun c1stack-pop (destination args)
  (let* ((frame-var-name (pop args))
        (frame-var (c1vref frame-var-name)))
    (c1stack-frame-pop-values frame-var destination)))

(defun c1apply-from-stack-frame (destination args)
  (let* ((frame-var-name (first args))
         (function (second args))
         (frame-var (c1vref frame-var-name)))
    (nconc (c1translate 'VALUE0 function)
           (c1stack-frame-apply frame-var 'VALUE0)
           (c1set-loc destination 'VALUES))))
