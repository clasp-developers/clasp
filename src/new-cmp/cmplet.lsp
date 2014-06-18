;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPLET  Let and Let*.
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    ECL is free software; you can redistribute it and/or modify it
;;;;    under the terms of the GNU Library General Public License as
;;;;    published by the Free Software Foundation; either version 2 of
;;;;    the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defun c1let (destination args)
  (c1let/let* destination T args))

(defun c1let* (destination args)
  (c1let/let* destination NIL args))

(defun c1let/let* (destination psetq-p args)
  (check-args-number (if psetq-p 'LET 'LET*) args 1)
  (multiple-value-bind (body ss ts is other-decls)
      (c1body (rest args) nil)
    (let* ((*cmp-env* (cmp-env-copy *cmp-env*))
           ;; If there is only one variable binding, we use LET* instead of LET
           (let-bindings (first args))
           (psetq-p (and psetq-p (rest let-bindings)))
           (var-form-pairs (parse-let let-bindings ss is ts other-decls))
           (body (create-temps-for-specials var-form-pairs body psetq-p))
           (compiled-pairs (compile-let-forms var-form-pairs psetq-p
                                              ss is ts other-decls))
           (compiled-body (c1decl-body destination other-decls body)))
      (loop with locals = '()
         with specials = '()
         with forms = '()
         with extras = '()
         for (v . f) in compiled-pairs
         do (if (member (var-kind v) '(SPECIAL GLOBAL))
		(push v specials)
	        (push v locals))
	 do (setf extras (nconc extras f))
         finally (return (nconc (c1bind locals)
                                extras
                                compiled-body
                                (c1unbind (nconc specials locals)))
                         )))))

(defun parse-let (var-assignment-pairs ss is ts other-decls)
  (flet ((in-read-only-decl-p (v other-decls)
           (dolist (i other-decls nil)
             (when (and (eq (car i) :READ-ONLY)
                        (member v (rest i)))
               (return t)))))
    (loop for x in var-assignment-pairs
       collect (let (name form)
                 (cond ((symbolp x)
                        (setf name x
                              form nil
                              x nil))
                       ((not (and (consp x) (or (endp (cdr x)) (endp (cddr x)))))
                        (cmperr "Syntax error in LET/LET* variable binding~&~8T~S" x))
                       (t
                        (setf name (first x)
                              form (rest x))))
                 (let ((v (c1make-var name ss is ts)))
                   (when (in-read-only-decl-p name other-decls)
                     (setf (var-read-only-p v) t))
                   (cons v (if form (first form) (default-init v))))))))

(defun create-temps-for-specials (var-form-pairs body psetq-p)
  ;; In a LET form, when special variables are bound they cause a side
  ;; effect. In order to keep the assignments really parallel, we have
  ;; to save the temporal values.
  (when psetq-p
    (loop with specials = '()
       for pair in var-form-pairs
       for var = (car pair)
       for form = (cdr pair)
       when (member (var-kind var) '(SPECIAL GLOBAL))
       do (let ((aux (c1make-var (gensym) nil nil nil)))
            (setf (car pair) aux)
            (push (list (var-name var) (var-name aux)) specials))
       finally (when specials
                 (setf body `((let* ,specials
                                (declare (special ,@(mapcar #'car specials)))
                                ,@body))))))
  body)

(defun compile-let-forms (var-form-pairs psetq-p ss is ts other-decls)
  ;; Compile the assigned forms. If the variables are sequentially
  ;; assigned, as in LET*, they are added one by one to the
  ;; environment. Otherwise PSETQ-P = T and the forms are compiled in
  ;; an environment without the variables.
  ;; For read only variables, we can change their values here.
  (loop with variable-names = nil
     for pair in var-form-pairs
     for v = (car pair)
     for name = (var-name v)
     for form = (cdr pair)
     for binding-type = (if (global-var-p v) 'c1bind-special 'c1translate)
     for compiled-form = (funcall binding-type v form)
     do (setf (cdr pair) compiled-form
              variable-names (cons name variable-names))
     do (unless psetq-p
          (cmp-env-register-var v))
     finally
       (progn
         (when psetq-p
           (loop for (v . form) in var-form-pairs
              do (cmp-env-register-var v)))
         (check-vdecl variable-names ts is)
         (c1declare-specials ss)))
  var-form-pairs)
