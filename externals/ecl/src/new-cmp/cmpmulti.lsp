;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPMULT  Multiple-value-call and Multiple-value-prog1.

(in-package "COMPILER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c1multiple-value-call (destination args &aux forms)
  (check-args-number 'MULTIPLE-VALUE-CALL args 1)
  (cond
   ;; (M-V-C #'FUNCTION) => (FUNCALL #'FUNCTION)
   ((endp (rest args)) (c1funcall destination args))
   ;; (M-V-C #'FUNCTION (VALUES A ... Z)) => (FUNCALL #'FUNCTION A ... Z)
   ((and (= (length args) 2)
	 (consp (setq forms (second args)))
	 (eq 'VALUES (first forms)))
    (c1funcall destination (list* (first args) (rest forms))))
   ;; More complicated case.
   (t
    (c1translate destination
                 (let ((function (gensym))
                       (frame (gensym)))
                   `(with-stack ,frame
                      (let* ((,function ,(first args)))
                        ,@(loop for i in (rest args)
                             collect `(stack-push-values ,frame ,i))
                        (si::apply-from-stack-frame ,frame ,function))))))))

(defun c1multiple-value-prog1 (destination args)
  (check-args-number 'MULTIPLE-VALUE-PROG1 args 1)
  (c1translate destination
               (let ((frame (gensym)))
                 `(with-stack ,frame
                    (stack-push-values ,frame ,(first args))
                    ,@(rest args)
                    (stack-pop ,frame)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Beppe:
;;; this is the WRONG way to handle 1 value problem.
;;; should be done in c2values, so that (values (truncate a b)) can
;;; be used to restrict to one value, so we would not have to warn
;;; if this occurred in a proclaimed fun.

(defun c1values (destination forms)
  (cond ((eq destination 'TRASH)
         ;; When the values are not going to be used, then just
         ;; process each form separately.
         (loop for f in forms nconc (c1translate 'TRASH f)))
        ;; For locations that involve multiple values, we must use
        ;; the values operator
        ((member destination '(RETURN VALUES VALUES+VALUE0))
         (c1with-saved-values (prefix postfix temps forms)
           (nconc prefix
                  (c1values-op temps)
                  (c1set-loc destination 'VALUES)
                  postfix)))
        ;; Otherwise we can just save the first value and trash the rest
        (t
         (unless forms (setf forms '(nil)))
         (c1with-temps (prefix postfix temp)
          (nconc prefix
                 (c1translate temp (pop forms))
                 (loop for f in forms
                    nconc (c1translate 'TRASH f))
                 (c1set-loc destination temp)
                 postfix)))))

(defun c1multiple-value-setq (destination args &aux
                              (vars nil) (temp-vars nil)
			      (late-bindings nil))
  (check-args-number 'MULTIPLE-VALUE-SETQ args 2 2)
  (dolist (var (reverse (first args)))
    (cmpck (not (symbolp var)) "The variable ~s is not a symbol." var)
    (setq var (chk-symbol-macrolet var))
    (cond ((symbolp var)
	   (cmpck (constantp var)
		  "The constant ~s is being assigned a value." var)
	   (push var vars))
	  (t (let ((new-var (gensym)))
	       (push new-var vars)
	       (push new-var temp-vars)
	       (push `(setf ,var ,new-var) late-bindings)))))
  (let ((value (second args)))
    (cond (temp-vars
	   (c1translate destination
                        `(let* (,@temp-vars)
                           (multiple-value-setq ,vars ,value)
                           ,@late-bindings)))
	  ((endp vars)
	   (c1translate destination `(values ,value)))
	  ((= (length vars) 1)
	   (c1translate destination `(setq ,(first vars) ,value)))
	  (t
           (setf vars (mapcar #'c1vref vars))
           (nconc (c1translate 'VALUES value)
                  (c1set-mv vars)
                  (c1var destination (var-name (first vars))))))))

(defun c1multiple-value-bind (destination args &aux
                              (vars nil) (vnames nil) init-form
                              ss is ts body other-decls
                              (*cmp-env* (cmp-env-copy)))
  (check-args-number 'MULTIPLE-VALUE-BIND args 2)
  (let* ((variables (pop args))
         (values (pop args))
         (body args))
    (c1translate
     destination
     (case (length variables)
       ;; Simple cases can be rewritten as more efficient forms
       ;; Note that without variables we need a body because otherwise
       ;; the VALUES form is output.
       (0 `(progn ,values ,@(or body '((progn)))))
       (1 `(let ((,(first variables) ,values)) ,@body))
       ;; FIXME! This is ugly ugly and it is there because we do not want
       ;; to duplicate the code in LET/LET* but surely there is a better
       ;; way.
       (t (let ((temps (loop for i in variables collect (gensym "VALUES"))))
            `(let ,temps
               (multiple-value-setq ,temps ,values)
               (let (,@(loop for a in variables
                          for b in temps
                          collect (list a b)))
                 ,@body))))))))
