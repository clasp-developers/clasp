;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPSPECIAL  Miscellaneous special forms.

;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.


(in-package "COMPILER")

(defun c1quote (destination args)
  (check-args-number 'QUOTE args 1 1)
  (c1constant-value destination (car args) :always t))

(defun c1declare (args)
  (cmperr "The declaration ~s was found in a bad place." (cons 'DECLARE args)))

(defun c1the (destination args)
  (check-args-number 'THE args 2 2)
  (let* ((type (pop args))
         (value (pop args)))
    (c1translate `(THE ,type ,destination) value)))

(defun c1with-backend (destination forms)
  (c1progn destination (loop for tag = (pop forms)
                          for form = (pop forms)
                          while tag
                          when (eq tag :c/c++)
                          collect form)))

(defun c1compiler-let (destination args &aux (symbols nil) (values nil))
  (when (endp args) (too-few-args 'COMPILER-LET 1 0))
  (dolist (spec (car args))
    (cond ((consp spec)
           (cmpck (not (and (symbolp (car spec))
                            (or (endp (cdr spec))
                                (endp (cddr spec)))))
                  "The variable binding ~s is illegal." spec)
           (push (car spec) symbols)
           (push (if (endp (cdr spec)) nil (eval (second spec))) values))
          ((symbolp spec)
           (push spec symbols)
           (push nil values))
          (t (cmperr "The variable binding ~s is illegal." spec))))
  (setq symbols (nreverse symbols))
  (setq values (nreverse values))
  (progv symbols values (c1progn destination (cdr args))))

(defun c1function (destination args &aux fd)
  (check-args-number 'FUNCTION args 1 1)
  (let ((fun (car args)))
    (cond ((si::valid-function-name-p fun)
	   (let ((funob (local-function-ref fun t)))
	     (if funob
                 (c1set-loc destination (fun-var funob))
                 (c1set-loc destination `(FDEFINITION ,fun)))))
          ((and (consp fun) (member (car fun) '(LAMBDA EXT::LAMBDA-BLOCK)))
           (cmpck (endp (cdr fun))
                  "The lambda expression ~s is illegal." fun)
	   (let (name body)
	     (if (eq (first fun) 'EXT::LAMBDA)
		 (setf name (gensym) body (rest fun))
		 (setf name (second fun) body (cddr fun)))
	     (let* ((funob (c1compile-function body :name name))
		    (lambda-form (fun-lambda funob)))
	       (setf (fun-ref-ccb funob) t)
	       (compute-fun-closure-type funob)
               (nconc (c1do-flet/labels-op (list funob))
                      (c1set-loc destination `(MAKE-CCLOSURE ,funob))))))
	  (t (cmperr "The function ~s is illegal." fun)))))

;;; Mechanism for sharing code.
(defun new-local (fun)
  ;; returns the previous function or NIL.
  (declare (type fun fun))
  (case (fun-closure fun)
    (CLOSURE
     (setf (fun-level fun) 0 (fun-env fun) *env*))
    (LEXICAL
     (let ((parent (fun-parent fun)))
       ;; Only increase the lexical level if there have been some
       ;; new variables created. This way, the same lexical environment
       ;; can be propagated through nested FLET/LABELS.
       (setf (fun-level fun) (if (plusp *lex*) (1+ *level*) *level*)
	     (fun-env fun) 0)))
    (otherwise
     (setf (fun-env fun) 0 (fun-level fun) 0)))
  (push fun *local-funs*))
