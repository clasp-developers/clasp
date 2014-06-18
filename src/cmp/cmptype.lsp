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

;;;; CMPTYPE  Type information.

(in-package "COMPILER")

;;;
;;; and-form-type
;;;   returns a copy of form whose type is the type-and of type and the form's
;;;   type
;;;
(defun and-form-type (type form original-form &optional (mode :safe)
		      (format-string "") &rest format-args)
  (let* ((type2 (c1form-primary-type form))
	 (type1 (type-and type type2)))
    ;; We only change the type if it is not NIL. Is this wise?
    (if type1
	(setf (c1form-type form) type1)
	(funcall (if (eq mode :safe) #'cmperr #'cmpwarn)
		 "~?, the type of the form ~s is ~s, not ~s." format-string
		 format-args original-form type2 type))
    form))

(defun default-init (var &optional warn)
  (declare (ignore warn))
  (let ((new-value (cdr (assoc (var-type var)
			       '((fixnum . 0) (character . #\space)
                                 #+long-float (long-float 0.0L1)
				 (double-float . 0.0D1) (single-float . 0.0F1))
			       :test #'subtypep))))
    (if new-value
	(c1constant-value new-value :only-small-values t)
        (c1nil))))

(defun expand-deftype (type)
  (let (base args)
    (if (atom type)
        (setf base type args nil)
        (setf base (car type) args (cdr type)))
    (let ((fn (get-sysprop base 'SI::DEFTYPE-DEFINITION)))
      (if fn
          (expand-deftype (apply fn args))
          type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TYPE CHECKING
;;

(defun lambda-type-check-associate (fname requireds optionals keywords global-fun-p)
  (multiple-value-bind (arg-types found)
      (and global-fun-p (get-arg-types fname *cmp-env* global-fun-p))
    (if found
        (multiple-value-bind (req-types opt-types rest-flag key-flag
                                        key-types allow-other-keys)
            (si::process-lambda-list arg-types 'ftype)
	  (declare (ignore rest-flag key-flag allow-other-keys))
          (nconc 
           (loop for var in requireds
              for type in (rest req-types)
              collect (cons var type))
           (loop for optional in optionals by #'cdddr
              for type in (rest opt-types) by #'cdddr
              collect (cons optional type))
           (loop for key-list on keywords by #'cddddr
              for keyword = (first key-list)
              for key-var = (second key-list)
              for type = (loop for key-list on (rest key-types) by #'cddr
                            when (eq keyword (first key-list))
                            return (second key-list)
                            finally (return t))
              collect (cons key-var type))))
        (nconc
         (loop for var in requireds
            collect (cons var t))
         (loop for optional in optionals by #'cdddr
            collect (cons optional t))
         (loop for key-list on keywords by #'cddddr
            for key-var = (second key-list)
            collect (cons key-var t))))))

(defun lambda-type-check-precise (assoc-list ts)
  (loop for record in assoc-list
     for var = (car record)
     for type = (assoc (var-name var) ts)
     when type
     do
     ;; Instead of trusting the global proclamation, we set a check based
     ;; on the local declaration, without type merging.
       (rplacd record (cdr type))
       #+(or)
       (rplacd record (type-and (cdr record) (cdr type))))
  assoc-list)

(defun extract-lambda-type-checks (fname requireds optionals keywords ts other-decls)
  ;; We generate automatic type checks for function arguments that
  ;; are declared These checks can be deactivated by appropriate
  ;; safety settings which are checked by ASSERT-TYPE. Note
  ;; that not all type declarations can be checked (take for instance
  ;; (type (function (t t) t) foo)) We let the macro do the job.
  (loop with policy-check-type = (policy-check-arguments-type)
     with checks = '()
     with new-auxs = '()
     with global-fun-p = (member '(si::c-global) other-decls :test #'equal)
     with type-checks = (lambda-type-check-precise
                         (lambda-type-check-associate fname requireds
                                                      optionals keywords
                                                      global-fun-p)
                         ts)
     for (var . type) in type-checks
     for name = (var-name var)
     ;; Non trivial types are the only ones we care about
     unless (eq type t)
     do (if (and policy-check-type
                 (loop for decl in other-decls
                    never (and (consp decl)
                               (eq (first decl)
                                   'si::no-check-type)
                               (member name (rest decl)))))
            ;; We remove assumption about types, which will be checked
            ;; later due to this assertion...
            (setf (var-type var) t
                  checks (list* `(type-assertion ,name ,type) checks)
                  new-auxs (list* `(truly-the ,type ,name) name new-auxs))
            ;; Or simply enforce the variable's type.
            (setf (var-type var) (type-and (var-type var) type)))
     finally
       (progn
         (when checks
           (cmpnote "In ~:[an anonymous function~;function ~:*~A~], checking types of argument~@[s~]~{ ~A~}."
                    (fun-name *current-function*)
                    (mapcar #'second checks)))
         (return (cons (nreverse checks) (nreverse new-auxs))))))

(defun type-error-check (value type)
  (case type
    (cons
     `(ffi:c-inline (,value) (:object) :void
        "@0;if (ecl_unlikely(ECL_ATOM(#0))) FEtype_error_cons(#0);"
        :one-liner nil))
    (array
     `(ffi:c-inline (,value) (:object) :void
        "if (ecl_unlikely(!ECL_ARRAYP(#0))) FEtype_error_array(#0);"
        :one-liner nil))
    (list
     `(ffi:c-inline (,value) (:object) :void
        "if (ecl_unlikely(!ECL_LISTP(#0))) FEtype_error_list(#0);"
        :one-liner nil))
    (sequence
     `(ffi:c-inline (,value) (:object) :void
        "if (ecl_unlikely(!(ECL_LISTP(#0) || ECL_VECTORP(#0))))
           FEtype_error_sequence(#0);"
        :one-liner nil))
    (otherwise
     `(ffi:c-inline
       ((typep ,value ',type) ',type ,value)
       (:bool :object :object) :void
       "if (ecl_unlikely(!(#0)))
         FEwrong_type_argument(#1,#2);" :one-liner nil))))

(defmacro assert-type-if-known (&whole whole value type &environment env)
  "Generates a type check on an expression, ensuring that it is satisfied."
  (multiple-value-bind (trivial valid)
      (subtypep 't type)
    (cond ((and trivial valid)
	   value)
	  ((multiple-value-setq (valid value) (constant-value-p value env))
	   (si::maybe-quote value))
	  (t
	   (with-clean-symbols (%value)
	     `(let* ((%value ,value))
		,(type-error-check '%value (replace-invalid-types type))
		(truly-the ,type %value)))))))

(defun replace-invalid-types (type)
  ;; Some types which are acceptable in DECLARE are not
  ;; accepted by TYPEP. We thus simplify the type replacing
  ;; the offending ones by more general types. No problem
  ;; doing this since the type checks are optional.
  (if (atom type)
      type
      (let ((name (car type)))
	(case name
	  (FUNCTION 'FUNCTION)
	  ((OR AND NOT CONS)
	   (list* name (mapcar #'replace-invalid-types (rest type))))
	  (otherwise
	   type)))))

(defmacro optional-type-check (&whole whole value type &environment env)
  (declare (ignore env))
  (if (policy-assume-right-type)
      value
      `(assert-type-if-known ,value ,type)))

(defmacro with-let*-type-check (triplets &body body)
  `(let* ,(loop for (var value type) in triplets
	     collect `(,var (assert-type-if-known ,value ,type)))
     (declare (:read-only ,@(mapcar #'car triplets)))
     ,@body))

