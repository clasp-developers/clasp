;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPPASS  Optimization passes
;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia Ripoll.
;;;;
;;;;    ECL is free software; you can redistribute it and/or modify it
;;;;    under the terms of the GNU Library General Public License as
;;;;    published by the Free Software Foundation; either version 2 of
;;;;    the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ALL C1FORMS
;;;
;;;	BIND			(var1 ... varN)
;;;	BIND-REQUIREDS		((var1 . arg1-loc) ... (varN . argN-loc))
;;;	BIND-SPECIAL		destination value-loc
;;;	CALL-LOCAL		destination fun (arg1 ... argN)
;;;	CALL-GLOBAL		destination fun (arg1 ... argN)
;;;	C-INLINE
;;;	DEBUG-ENV-OPEN		fun-name
;;;	DEBUG-ENV-PUSH-VARS	(var1 ... varN)
;;;	DEBUG-ENV-POP-VARS	(var1 ... varN) close-block
;;;	DEBUG-ENV-CLOSE		fun-name
;;;	DO-FLET/LABELS		(fun1 ... funN)
;;;	FRAME-ID		frame-var
;;;	FRAME-JMP-NEXT		frame-var
;;;	FRAME-POP		frame-var
;;;	FRAME-SAVE-NEXT		frame-var
;;;	FRAME-SET		id-loc no-label
;;;	FUNCALL			destination (arg1 ... argN)
;;;	FUNCTION-PROLOGUE	fun
;;;	FUNCTION-EPILOGUE	fun
;;;	GO			tag
;;;	JMP			tag
;;;	PROGV			ndx-loc (var1-loc ... varN-loc) values-loc
;;;	PROGV-EXIT		ndx-loc
;;;	SET			destination source
;;;	SET-MV			(dest-loc1 ... dest-locN) min-args max-args
;;;	SI:STRUCTURE-REF
;;;	SI:STRUCTURE-SET
;;;	STACK-FRAME-OPEN	frame-var
;;;	STACK-FRAME-PUSH	frame-var value-loc
;;;	STACK-FRAME-PUSH-VALUES	frame-var
;;;	STACK-FRAME-POP-VALUES	frame-var
;;;	STACK-FRAME-APPLY	frame-var fun-loc
;;;	STACK-FRAME-CLOSE	frame-var
;;;	RETURN-FROM		block-id-var block-name
;;;	THROW			tag-loc
;;;	UNBIND			(var1 ... varN)
;;;	VALUES			(value1-loc ... valueN-loc)
;;;	VARARGS-BIND		nargs-loc varargs-loc min max nkeys check
;;;	VARARGS-POP		dest-loc nargs-loc varargs-loc
;;;	VARARGS-REST		dest-loc nargs-loc varargs-loc nkeys
;;;				keys-list-loc allow-other-keys
;;;	VARARGS-UNBIND		nargs-loc varargs-loc min max nkeys check
;;;

(in-package "C-PASSES")

(defparameter *dump-output* (open "dump.log" :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create))

(defun execute-pass (pass)
  (format *dump-output* "~&;;; Executing pass ~A" pass)
  (loop with pending = (list *top-level-forms*)
     for f = (pop pending)
     for *current-function* = f
     while f
     do (let ((*compile-file-truename* (fun-file f))
              (*compile-file-position* (fun-file-position f))
              (*current-toplevel-form* (fun-toplevel-form f)))
          (format *dump-output* "~&;;; Executing pass ~A on ~A" pass f)
          (setf (fun-lambda f) (funcall pass f (fun-lambda f))
                pending (append (fun-child-funs f) pending)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DELETE UNUSED FORMS
;;;

(defun pass-delete-no-side-effects (function forms)
  "Going backwards, we examime forms that cause no side effects and whose
output value is not used."
  (nreverse (delete-if #'delete-if-no-side-effects
                       (nreverse forms))))

(defun delete-if-no-side-effects (form)
  (when (c1form-p form)
    (case (c1form-name form)
      ((LOCATION VAR SYS:STRUCTURE-REF #+clos SYS:INSTANCE-REF)
       t)
      ((BIND UNBIND)
       (every #'unused-variable-p (c1form-arg 0 form)))
      ((VARARGS-REST VARARGS-POP)
       (let ((destination (c1form-arg 0 form)))
         (when (unused-variable-p destination)
           (setf (c1form-arg 0 form) 'TRASH)
           (eliminate-from-set-nodes destination form)))
       nil)
      (CALL-GLOBAL
       (let* ((form-args (c1form-args form))
              (destination (first form-args))
              (fname (second form-args))
              (args (third form-args)))
         (cond ((function-may-have-side-effects fname)
                nil)
               ((unused-destination destination)
                (loop for i in args do (eliminate-from-read-nodes i form))
                (eliminate-from-set-nodes destination form)
                t)
               (t nil))))
      (SET
       (let* ((destination (c1form-arg 0 form))
              (source (c1form-arg 1 form)))
         (when (unused-destination destination)
           (eliminate-from-set-nodes destination form)
           (eliminate-from-read-nodes destination form)
           (eliminate-from-set-nodes source form)
           (eliminate-from-read-nodes source form)
           t)))
      (t nil))))

(defun unused-destination (dest)
  (or (eq dest 'trash)
      (and (var-p dest)
           (unused-variable-p dest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DELETE UNUSED BINDINGS
;;;

(defun pass-delete-unused-bindings (function forms)
  "We eliminate all unused variables, including their bindings. Empty BIND/UNBIND
forms are also suppressed."
  (labels ((unused-variable-binding-p (v)
             (unused-variable-p (if (consp v) (car v) v)))
           (unused-bindings (form)
             (and (c1form-p form)
                  (member (c1form-name form) '(BIND UNBIND BIND-REQUIREDS
                                               DEBUG-ENV-PUSH-VARS
                                               DEBUG-ENV-POP-VARS))
                  (let ((new-args (delete-if #'unused-variable-binding-p
                                             (c1form-arg 0 form))))
                    (setf (c1form-args form) (list new-args))
                    (null new-args)))))
    (when (warn-about-unused-variables function)
      (setf (fun-local-vars function)
            (delete #'unused-variable-p (fun-local-vars function))
            (fun-referred-vars function)
            (delete #'unused-variable-p (fun-referred-vars function))))
    (delete-if #'unused-bindings forms)))

(defun warn-about-unused-variables (function)
  (loop with *current-function* = function
     with unused-to-be-warned = '()
     for v in (fun-local-vars function)
     when (and (unused-variable-p v)
               (zerop (var-ref v)) ; VAR-REF < 0 means ignore
               (not (or (global-var-p v)
                        (temporal-var-p v))))
     do (push v unused-to-be-warned)
     finally (return
               (when unused-to-be-warned
                 (cmpwarn-style "In function ~S found unused variables:~&~4T~{ ~S~}."
                                (fun-name function)
                                (mapcar #'var-name unused-to-be-warned))
                 t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONSISTENCY CHECKS
;;;

(defun pass-consistency (function forms)
  "We verify that all used variables that appear in a form contain this
form in its read/set nodes, and add other consistency checks."
  (pprint-c1forms forms *dump-output*)
  (labels ((in-read-set-nodes (tree form)
             (cond ((var-p tree)
                    (or (member form (var-read-nodes tree) :test #'eq)
                        (member form (var-set-nodes tree) :test #'eq)))
                   ((atom tree)
                    t)
                   (t
                    (and (in-read-set-nodes (car tree) form)
                         (in-read-set-nodes (cdr tree) form))))))
    (loop for form in forms
       for args = (and (c1form-p form) (c1form-args form))
       unless (or (null args)
                  (in-read-set-nodes args form)
                  (member (c1form-name form)
                          '(BIND UNBIND BIND-REQUIREDS
                            BIND-SPECIAL PROGV
                            FRAME-JMP-NEXT
                            FRAME-POP FRAME-SAVE-NEXT
                            VARARGS-BIND VARARGS-UNBIND)))
       do (progn
            (pprint-c1forms forms)
            (error ";;; Inconsistent form ~S form with arguments~{~&;;;   ~A~}"
                   (c1form-name form) args)))
    forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ASSIGN LABELS
;;;

(defun pass-assign-labels (function forms)
  (loop with *last-label* = 0
     with last-tag = nil
     with last = nil
     for f in forms
     do (cond ((not (tag-p f))
               (setf last nil))
              (last
               (setf (tag-label f) last
                     (tag-ref last-tag) (+ (tag-ref last-tag) (tag-ref f))
                     (tag-ref f) 0))
              (t
               (setf last-tag f
                     last (next-label)
                     (tag-label f) last)))
     finally (setf (fun-last-label function) *last-label*))
  forms)
