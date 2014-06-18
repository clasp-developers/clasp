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

;;;; CMPINLINE  Open coding optimizer.

(in-package "COMPILER")

;;; Valid property names for open coded functions are:
;;;  :INLINE-ALWAYS
;;;  :INLINE-SAFE	safe-compile only
;;;  :INLINE-UNSAFE	non-safe-compile only
;;;
;;; Each property is a list of 'inline-info's, where each inline-info is:
;;; ( types { type | boolean } { string | function } ).
;;;
;;; For each open-codable function, open coding will occur only if there exits
;;; an appropriate property with the argument types equal to 'types' and with
;;; the return-type equal to 'type'.  The third element
;;; is T if and only if side effects may occur by the call of the function.
;;; Even if *DESTINATION* is TRASH, open code for such a function with side
;;; effects must be included in the compiled code.
;;; The forth element is T if and only if the result value is a new Lisp
;;; object, i.e., it must be explicitly protected against GBC.

(defun make-inline-temp-var (value-type &optional rep-type)
  (let ((out-rep-type (or rep-type (lisp-type->rep-type value-type))))
    (if (eq out-rep-type :object)
        (make-temp-var)
        (let ((var (make-lcl-var :rep-type out-rep-type
                                 :type value-type)))
	  (open-inline-block)
          (wt-nl (rep-type->c-name out-rep-type) " " var ";")
          var))))

(defun save-inline-loc (loc)
  (let* ((rep-type (loc-representation-type (second loc)))
         (temp (make-inline-temp-var (first loc) rep-type))
         (*destination* temp))
    (set-loc loc)
    temp))

(defmacro with-inlined-loc ((temp-loc loc) &rest body)
  `(let ((,temp-loc (save-inline-loc ,loc)))
     (setf ,temp-loc (list (var-type ,temp-loc) ,temp-loc))
     ,@body))

(defun emit-inlined-variable (form rest-forms)
  (let ((var (c1form-arg 0 form))
        (value-type (c1form-primary-type form)))
    (if (var-changed-in-form-list var rest-forms)
        (let* ((temp (make-inline-temp-var value-type (var-rep-type var))))
          (let ((*destination* temp)) (set-loc var))
          (list value-type temp))
        (list value-type var))))

(defun emit-inlined-setq (form rest-forms)
  (let ((vref (c1form-arg 0 form))
        (form1 (c1form-arg 1 form)))
    (let ((*destination* vref)) (c2expr* form1))
    (if (eq (c1form-name form1) 'LOCATION)
        (list (c1form-primary-type form1) (c1form-arg 0 form1))
        (emit-inlined-variable (make-c1form 'VAR form vref) rest-forms))))

(defun emit-inlined-call-global (form expected-type)
  (let* ((fname (c1form-arg 0 form))
         (args (c1form-arg 1 form))
         (return-type (c1form-primary-type form))
         (fun (find fname *global-funs* :key #'fun-name :test #'same-fname-p))
         (loc (call-global-loc fname fun args return-type expected-type))
         (type (type-and return-type (loc-type loc)))
         (temp (make-inline-temp-var type (loc-representation-type loc)))
         (*destination* temp))
    (set-loc loc)
    (list type temp)))

(defun emit-inlined-progn (form forms)
  (let ((args (c1form-arg 0 form)))
    (loop with *destination* = 'TRASH
       while (rest args)
       do (c2expr* (pop args)))
    (emit-inline-form (first args) forms)))

(defun emit-inlined-values (form forms)
  (let ((args (c1form-arg 0 form)))
    (prog1 (emit-inline-form (or (pop args) (c1nil)) forms)
      (loop with *destination* = 'TRASH
         for form in args
         do (c2expr* form)))))

(defun emit-inlined-structure-ref (form rest-forms)
  (let ((type (c1form-primary-type form)))
    (if (args-cause-side-effect rest-forms)
        (let* ((temp (make-inline-temp-var type :object))
               (*destination* temp))
          (c2expr* form)
          (list type temp))
        (list type
              (list 'SYS:STRUCTURE-REF
                    (first (coerce-locs
                            (inline-args (list (c1form-arg 0 form)))))
                    (c1form-arg 1 form)
                    (c1form-arg 2 form)
                    (c1form-arg 3 form))))))

(defun emit-inlined-instance-ref (form rest-forms)
  (let ((type (c1form-primary-type form)))
    (if (args-cause-side-effect rest-forms)
        (let* ((temp (make-inline-temp-var type :object))
               (*destination* temp))
          (c2expr* form)
          (list type temp))
        (list type
              (list 'SYS:INSTANCE-REF
                    (first (coerce-locs
                            (inline-args (list (c1form-arg 0 form)))))
                    (c1form-arg 1 form)
                    #+nil (c1form-arg 2 form))))))

(defun emit-inline-form (form forms)
  (with-c1form-env (form form)
    (case (c1form-name form)
      (LOCATION
       (list (c1form-primary-type form) (c1form-arg 0 form)))
      (VAR
       (emit-inlined-variable form forms))
      (CALL-GLOBAL
       (emit-inlined-call-global form (c1form-primary-type form)))
      (SYS:STRUCTURE-REF
       (emit-inlined-structure-ref form forms))
      #+clos
      (SYS:INSTANCE-REF
       (emit-inlined-instance-ref form forms))
      (SETQ
       (emit-inlined-setq form forms))
      (PROGN
        (emit-inlined-progn form forms))
      (VALUES
       (emit-inlined-values form forms))
      (t (let* ((type (c1form-primary-type form))
                (temp (make-inline-temp-var type)))
           (let ((*destination* temp)) (c2expr* form))
           (list type temp))))))

;;;
;;; inline-args:
;;;   returns a list of pairs (type loc)
;;;   side effects: emits code for temporary variables
;;;
;;; Whoever calls inline-args must bind *inline-blocks* to 0 and afterwards
;;; call close-inline-blocks
;;;
(defun inline-args (forms)
  (loop for form-list on forms
     for form = (first form-list)
     collect (emit-inline-form form (rest form-list))))

(defun destination-type ()
  (rep-type->lisp-type (loc-representation-type *destination*))
  ;;(loc-type *destination*)
)

(defun maybe-open-inline-block ()
  (unless (plusp *inline-blocks*)
    (open-inline-block)))

(defun open-inline-block ()
  (wt-nl-open-brace)
  (incf *inline-blocks*))

(defun close-inline-blocks (&optional new-line)
  (loop for i of-type fixnum from 0 below *inline-blocks*
     do (wt-nl-close-brace)))

(defun form-causes-side-effect (form)
  (c1form-side-effects form))

(defun args-cause-side-effect (forms)
  (some #'c1form-side-effects forms))

(defun function-may-have-side-effects (fname)
  (not (get-sysprop fname 'no-side-effects)))

(defun function-may-change-sp (fname)
  (not (or (get-sysprop fname 'no-side-effects)
	   (get-sysprop fname 'no-sp-change))))
