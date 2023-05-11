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
  (funcall #'(setf macro-function)
           #'(lambda (def env)
               (declare (core:lambda-name in-package))
	       `(eval-when (eval compile load)
		  (si::select-package ,(string (second def)))))
           'in-package)
)

;;
;; This is also needed for booting Clasp. In particular it is required in
;; defmacro.lisp.
;;

;; Required by REGISTER-GLOBAL in cmp/cmpvar.lisp
(funcall #'(setf macro-function)
         #'(lambda (w e)
             (declare (ignore e)
                      (core:lambda-name pushnew)
                      (core:lambda-list item place))
             (let ((item (cadr w))
                   (place (caddr w)))
               `(setq ,place (adjoin ,item ,place))))
         'pushnew)

(funcall #'(setf macro-function)
         #'(lambda (w e)
             (declare (ignore e))
             (let ((item (cadr w))
                   (place (caddr w)))
               `(setq ,place (cons ,item ,place))))
         'push)

(funcall #'(setf macro-function)
         #'(lambda (def env)
             (declare (ignore env)
                      (core:lambda-name when)
                      (core:lambda-list condition &body forms))
             `(if ,(cadr def) (progn ,@(cddr def))))
         'when)


(funcall #'(setf macro-function)
         #'(lambda (def env)
             (declare (ignore env)
                      (core:lambda-name unless)
                      (core:lambda-list condition &body forms))
             `(if ,(cadr def) nil (progn ,@(cddr def))))
         'unless)


(defun si::while-until (test body jmp-op)
  (let ((label (gensym))
        (exit (gensym)))
    `(TAGBODY
        (GO ,exit)
      ,label
        ,@body
      ,exit
        (,jmp-op ,test (GO ,label)))))

(funcall #'(setf macro-function)
         #'(lambda (def env)
             (declare (ignore env))
             (si::while-until (cadr def) (cddr def) 'when))
         'si::while)

(funcall #'(setf macro-function)
         #'(lambda (def env)
             (declare (ignore env))
             (si::while-until (cadr def) (cddr def) 'unless))
         'si::until)


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
             (declare (ignore env) (core:lambda-name dolist)
                      (core:lambda-list
                       ((var list-form &optional result-form) &body body)))
             (let (body control var expr exit)
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
                    (let* ((%dolist-var ,expr))
                      (si::while %dolist-var
                        (let ((,var (first %dolist-var)))
                          (declare ,@declarations)
                          (tagbody
                             ,@body
                             (setq %dolist-var (cdr %dolist-var))))))
                    ,(when exit
                       `(let ((,var nil))
                          (declare (ignorable ,var)
                                   ,@(filter-dolist-declarations declarations))
                          ,@exit))))))))
  (funcall #'(setf macro-function) f 'dolist))

(let ((f #'(lambda (whole env)
             (declare (ignore env) (core:lambda-name dotimes)
                      (core:lambda-list
                       ((var count-form &optional result-form) &body body)))
             (let (body control var expr exit)
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
                 (when (and (integerp expr) (>= expr 0))
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
  (funcall #'(setf macro-function) f 'dotimes))

(let ((f #'(lambda (whole env)
             (declare (ignore env) (core:lambda-name do/do*-expand)
                      (core:lambda-list (vars test &body body)))
             (let (do/do* control test result vlexport step let psetq body)
               (setq do/do* (first whole) body (rest whole))
               (if (eq do/do* 'do)
                   (setq let 'LET psetq 'PSETQ)
                   (setq let 'LET* psetq 'SETQ))
               (when (endp body)
                 (simple-program-error "Syntax error first (endp body) in ~A:~%~A" do/do* whole))
               (setq control (first body) body (rest body))
               (when (endp body)
                 (simple-program-error "Syntax error second (endp body) in ~A:~%~A" do/do* whole))
               (setq test (first body) body (rest body))
               (when (endp test)
                 (simple-program-error "Syntax error (endp test) in ~A:~%~A" do/do* whole))
               (setq result (rest test) test (first test))
               (dolist (c control)
                 (when (symbolp c) (setq c (list c)))
                 (let ((lenc (length c)))
                   (cond
                     ((or (eql lenc 1) (eql lenc 2))
                      (setq vlexport (cons c vlexport)))
                     ((eql lenc 3)
                      (setq vlexport (cons (butlast c) vlexport)
                            step (list* (third c) (first c) step)))
                     (t
                      (simple-program-error "Syntax error (length not 1,2,3 - its ~a and c is ~s) in ~A:~%~A" (length c) c do/do* whole)))))
               (multiple-value-bind (declarations real-body)
                   (process-declarations body nil)
                 `(BLOCK NIL
                    (,let ,(nreverse vlexport)
                      (declare ,@declarations)
                      (sys::until ,test
                                  ,@real-body
                                  ,@(when step (list (cons psetq (nreverse step)))))
                      ,@(or result '(nil)))))))))
  (funcall #'(setf macro-function) f 'do)
  (funcall #'(setf macro-function) f 'do*))

(funcall #'(setf macro-function)
         #'(lambda (whole env)
             (declare (ignore env))
             (let ((sym (gensym))
                   (first (cadr whole))
                   (body (cddr whole)))
               (if body
                   `(let ((,sym ,first))
                      ,@body
                      ,sym)
                   first)))
         'prog1)
