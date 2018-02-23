;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;                    Exporting external symbols of LISP package

(eval-when (eval compile load)
  (si::select-package "SI"))


;; This is needed only when bootstrapping CLASP using CLASP-MIN
(eval-when (eval)
  (si::fset 'in-package
            #'(lambda (def env)
                (declare (core:lambda-name in-package))
		`(eval-when (eval compile load)
		   (si::select-package ,(string (second def)))))
	    t)
)

;;
;; This is also needed for booting Clasp. In particular it is required in
;; defmacro.lsp.
;;

(defun filter-dolist-declarations (declarations)
  (let ((a nil))
    (mapc #'(lambda (clause)
              (when (not (and (consp clause)
                              (or (eq (car clause) 'type)
                                  (eq (car clause) 'ignore))))
                (setq a (cons clause a))))
          declarations)
    (nreverse a)))

(let ((f #'(lambda (whole env)
             (declare (ignore env) (core:lambda-name dolist))
             (let (body pop finished control var expr exit)
               (setq body (rest whole))
               (when (endp body)
                 (simple-program-error "Syntax error in ~A:~%~A" 'DOLIST whole))
               (setq control (first body) body (rest body))
               (when (endp control)
                 (simple-program-error "Syntax error in ~A:~%~A" 'DOLIST whole))
               (setq var (first control) control (rest control))
               (if (<= 1 (length control) 2)
                   (setq expr (first control) exit (rest control))
                   (simple-program-error "Syntax error in ~A:~%~A" 'DOLIST whole))
               (multiple-value-bind (declarations body)
                   (process-declarations body nil)
                 `(block nil
                    (let* ((%dolist-var ,expr)
                           ,var)
                      (declare ,@declarations)
                      (si::while %dolist-var
                                 (setq ,var (first %dolist-var))
                                 ,@body
                                 (setq %dolist-var (cons-cdr %dolist-var)))
                      ,(when exit `(setq ,var nil))
                      ,@exit)))))))
  (si::fset 'dolist f t nil '((var list-form &optional result-form) &body body)))

(let ((f #'(lambda (whole env)
             (declare (ignore env) (core:lambda-name dotimes))
             (let (body pop finished control var expr exit)
               (setq body (rest whole))
               (when (endp body)
                 (simple-program-error "Syntax error in ~A:~%~A" 'DOTIMES whole))
               (setq control (first body) body (rest body))
               (when (endp control)
                 (simple-program-error "Syntax error in ~A:~%~A" 'DOTIMES whole))
               (setq var (first control) control (rest control))
               (if (<= 1 (length control) 2)
                   (setq expr (first control) exit (rest control))
                   (simple-program-error "Syntax error in ~A:~%~A" 'DOTIMES whole))
               (multiple-value-bind (declarations body)
                   (process-declarations body nil)
                 (when (integerp expr)
                   (setq declarations
                         (cons `(type (integer 0 ,expr) ,var) declarations)))
                 `(block nil
                    (let* ((%dotimes-var ,expr)
                           (,var 0))
                      (declare ,@declarations)
                      (si::while (< ,var %dotimes-var)
                                 ,@body
                                 (setq ,var (1+ ,var)))
                      ,@exit)))))))
  (si::fset 'dotimes f t nil '((var count-form &optional result-form) &body body)))

(let ((f #'(lambda (whole env)
             (declare (ignore env) (core:lambda-name do/do*-expand))
             (let (do/do* control test result vl step let psetq body)
               (setq do/do* (first whole) body (rest whole))
               (if (eq do/do* 'do)
                   (setq let 'LET psetq 'PSETQ)
                   (setq let 'LET* psetq 'SETQ))
               (when (endp body)
                 (simple-program-error "Syntax error in ~A:~%~A" do/do* whole))
               (setq control (first body) body (rest body))
               (when (endp body)
                 (simple-program-error "Syntax error in ~A:~%~A" do/do* whole))
               (setq test (first body) body (rest body))
               (when (endp test)
                 (simple-program-error "Syntax error in ~A:~%~A" do/do* whole))
               (setq result (rest test) test (first test))
               (dolist (c control)
                 (when (symbolp c) (setq c (list c)))
                 (case (length c)
                   ((1 2)
                    (setq vl (cons c vl)))
                   (3
                    (setq vl (cons (butlast c) vl)
                          step (list* (third c) (first c) step)))
                   (t
                    (simple-program-error "Syntax error in ~A:~%~A" do/do* whole))))
               (multiple-value-bind (declarations real-body)
                   (process-declarations body nil)
                 `(BLOCK NIL
                    (,let ,(nreverse vl)
                      (declare ,@declarations)
                      (sys::until ,test
                                  ,@real-body
                                  ,@(when step (list (cons psetq (nreverse step)))))
                      ,@(or result '(nil)))))))))
  (si::fset 'do f t t '(vars test &body body))
  (si::fset 'do* f t t '(vars test &body body)))
