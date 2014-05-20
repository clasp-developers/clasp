;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPC-PASS  Optimization specific to each backend
;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia Ripoll.
;;;;
;;;;    ECL is free software; you can redistribute it and/or modify it
;;;;    under the terms of the GNU Library General Public License as
;;;;    published by the Free Software Foundation; either version 2 of
;;;;    the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "C-BACKEND")

(defun c-backend-passes ()
  (replace-optimizable-constants)
  (execute-pass 'pass-consistency)
  (execute-pass 'pass-delete-no-side-effects)
  (execute-pass 'pass-delete-unused-bindings)
  (execute-pass 'pass-assign-labels)
  (execute-pass 'pass-decide-var-rep-types))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DECIDE VARIABLE REPRESENTATION TYPE
;;;
;;; Currently this is very very crude:
;;;
;;;	* We unbox everything that is declared with a type that fits
;;;	  in a C variable.
;;;	* We cannot unbox function arguments, because they are always
;;;	  passed with C type cl_object.
;;;

(defun pass-decide-var-rep-types (function forms)
  (flet ((compute-variable-rep-type (v requireds)
           (let* ((kind (var-kind v)))
             (cond ((or (not (eq kind 'lexical)) (var-ref-clb v))
                    kind)
                   ((member v requireds :test #'eq)
                    :OBJECT)
                   (t
                    (lisp-type->rep-type (var-type v)))))))
    (loop with lambda-list = (fun-lambda-list function)
       with requireds = (first lambda-list)
       for v in (fun-local-vars function)
       do (setf (var-kind v) (compute-variable-rep-type v requireds))))
  forms)
