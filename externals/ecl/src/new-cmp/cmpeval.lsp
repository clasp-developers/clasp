;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;; CMPEVAL --  The Expression Dispatcher.

;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    ECoLisp is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defun c1expr (destination form)
  (setq form (catch *cmperr-tag*
    (cond ((symbolp form)
	   (setq form (chk-symbol-macrolet form))
	   (cond ((not (symbolp form))
		  (c1expr destination form))
		 ((eq form nil) (c1nil destination))
		 ((eq form t) (c1t destination))
		 ((keywordp form)
                  (c1set-loc destination (add-symbol form)))
		 ((constantp form)
		  (or (c1constant-value destination (symbol-value form)
                                        :only-small-values t)
		      (c1var destination form)))
		 (t (c1var destination form))))
          ((tag-p form)
           form)
	  ((consp form)
	   (let* ((fun (car form))
                  (*current-form* form))
	     (cond ((symbolp fun)
		    (c1call-symbol destination fun (cdr form)))
		   ((and (consp fun) (eq (car fun) 'LAMBDA))
		    (c1funcall destination form))
		   (t (cmperr "~s is not a legal function name." fun)))))
	  (t (c1constant-value destination form :always t)))))
  (if (eq form '*cmperr-tag*)
      (c1nil destination)
      form))

(defun c1nil (destination)
  (c1set-loc destination nil))
(defun c1t (destination)
  (c1set-loc destination t))

(defun c1constant-value (destination val &key always only-small-values)
  (when (eq destination 'TRASH)
    (return-from c1constant-value (c1nil destination)))
  (multiple-value-bind (loc found-p)
      (build-constant-value-loc val :always always :only-small-values only-small-values)
    (when found-p
      (c1set-loc destination loc))))

(defun build-constant-value-loc (val &key always only-small-values)
  (cond
   ((eq val nil) (values nil t))
   ((eq val t) (values t t))
   ((sys::fixnump val) (values (list 'FIXNUM-VALUE val) t))
   ((characterp val) (values (list 'CHARACTER-VALUE (char-code val)) t))
   ((typep val 'DOUBLE-FLOAT)
    (values (list 'DOUBLE-FLOAT-VALUE val (add-object val)) t))
   ((typep val 'SINGLE-FLOAT)
    (values (list 'SINGLE-FLOAT-VALUE val (add-object val)) t))
   ((typep val 'LONG-FLOAT)
    (values (list 'LONG-FLOAT-VALUE val (add-object val)) t))
   (always
    (values (add-object val) t))
   (t (values nil nil))))

(defun c1call-symbol (destination fname args &aux fd basic-fd)
  (cond ((and (setq basic-fd (gethash fname +c1-dispatch-table+))
              (special-operator-p fname))
         (funcall basic-fd destination args))
	((c1call-local destination fname args))
	((setq fd (cmp-env-search-macro fname))
	 (c1expr destination (cmp-expand-macro fd (list* fname args))))
	((and basic-fd (inline-possible fname))
	 (funcall basic-fd destination args))
	((and (setq fd (compiler-macro-function fname))
	      (inline-possible fname)
	      (let ((success nil))
		(multiple-value-setq (fd success)
		  (cmp-expand-macro fd (list* fname args)))
		success))
	 (c1expr destination fd))
	((setq fd (cmp-macro-function fname))
	 (c1expr destination (cmp-expand-macro fd (list* fname args))))
	(t (c1call-global destination fname args))))

(defun c1call-local (destination fname args)
  (let ((fun (local-function-ref fname)))
    (when fun
      (when (> (length args) si::c-arguments-limit)
	(return-from c1call-local
          (unoptimized-long-call destination `#',fname args)))
      (c1with-saved-values (prefix postfix temps args)
        (nconc prefix
               (c1call-local-op destination fun temps)
               postfix)))))

(defun c1call-global (destination fname args)
  (let ((l (length args))
	forms)
    (cond ((> l si::c-arguments-limit)
	   (unoptimized-long-call destination `#',fname args))
#|
	  ((maybe-optimize-structure-access destination fname args))
	  #+clos
	  ((maybe-optimize-generic-function destination fname args))
|#
	  (t
           (c1with-saved-values (prefix postfix temps args)
             (nconc prefix
                    (c1call-global-op destination fname temps)
                    postfix))))))

;;; ----------------------------------------------------------------------

(defvar *compiler-temps*
	'(tmp0 tmp1 tmp2 tmp3 tmp4 tmp5 tmp6 tmp7 tmp8 tmp9))

(defmacro sys::define-inline-function (name vars &body body)
  (let ((temps nil)
	(*compiler-temps* *compiler-temps*))
    (dolist (var vars)
      (if (and (symbolp var)
	       (not (member var '(&OPTIONAL &REST &KEY &AUX) :test #'eq)))
	(push (or (pop *compiler-temps*)
		  (gentemp "TMP" (find-package 'COMPILER)))
	      temps)
	(error "The parameter ~s for the inline function ~s is illegal."
	       var name)))
    (let ((binding (cons 'LIST (mapcar
				#'(lambda (var temp) `(list ',var ,temp))
				vars temps))))
      `(progn
	 (defun ,name ,vars ,@body)
	 (define-compiler-macro ,name ,temps (list* 'LET ,binding ',body))))))
