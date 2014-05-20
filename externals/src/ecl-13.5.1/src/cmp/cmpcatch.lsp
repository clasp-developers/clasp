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

;;;; CMPCATCH  Catch, Unwind-protect, and Throw.

(in-package "COMPILER")

(defun c1catch (args)
  (check-args-number 'CATCH args 1)
  (incf *setjmps*)
  (make-c1form* 'CATCH :sp-change t :type t :args (c1expr (first args))
		(c1progn (rest args))))

(defun c2catch (c1form tag body)
  (declare (ignore c1form))
  (let* ((new-destination (tmp-destination *destination*))
	 (code (incf *last-label*)))
    (let ((*destination* 'VALUE0))
      (c2expr* tag))
    (let* ((*destination* new-destination)
	   (*unwind-exit* (cons 'FRAME *unwind-exit*)))
      (if (member new-destination '(TRASH VALUES))
	  (progn
	    (wt-nl "if (ecl_frs_push(cl_env_copy," 'VALUE0 ")==0) {")
	    (wt-comment "BEGIN CATCH ~A" code)
	    (with-indentation
		(c2expr* body)))
	  (progn
	    (wt-nl "if (ecl_frs_push(cl_env_copy," 'VALUE0 ")) {")
	    (wt-comment "BEGIN CATCH ~A" code)
	    (with-indentation
		(with-exit-label (label)
		  (let ((*exit* label))
		    (unwind-exit 'VALUES))))
	    (wt-nl "} else {")
	    (with-indentation
		(c2expr* body)))))
    (wt-nl "}")
    (wt-nl "ecl_frs_pop(cl_env_copy);")
    (wt-comment "END CATCH ~A" code)
    (unwind-exit new-destination)))

(defun c1unwind-protect (args)
  (check-args-number 'UNWIND-PROTECT args 1)
  (incf *setjmps*)
  (let ((form (let ((*cmp-env* (cmp-env-mark 'UNWIND-PROTECT)))
                (c1expr (first args)))))
    (make-c1form* 'UNWIND-PROTECT :type (c1form-type form) :sp-change t
		  :args form (c1progn (rest args)))))

(defun c2unwind-protect (c1form form body)
  (declare (ignore c1form))
  (let* ((sp (make-lcl-var :rep-type :cl-index))
	 (nargs (make-lcl-var :rep-type :cl-index))
	 (*unwind-exit* `((STACK ,sp) ,@*unwind-exit*)))
    (wt-nl-open-brace)
    (wt-nl "volatile bool unwinding = FALSE;")
    (wt-nl "cl_index " sp "=ECL_STACK_INDEX(cl_env_copy)," nargs ";")
    (wt-nl "ecl_frame_ptr next_fr;")
    ;; Here we compile the form which is protected. When this form
    ;; is aborted, it continues at the ecl_frs_pop() with unwinding=TRUE.
    (wt-nl "if (ecl_frs_push(cl_env_copy,ECL_PROTECT_TAG)) {")
    (wt-nl "  unwinding = TRUE; next_fr=cl_env_copy->nlj_fr;")
    (wt-nl "} else {")
    (let ((*unwind-exit* (cons 'FRAME *unwind-exit*))
	  (*destination* 'VALUES))
      (c2expr* form))
    (wt-nl "}")
    (wt-nl "ecl_frs_pop(cl_env_copy);")
    ;; Here we save the values of the form which might have been
    ;; aborted, and execute some cleanup code. This code may also
    ;; be aborted by some control structure, but is not protected.
    (wt-nl nargs "=ecl_stack_push_values(cl_env_copy);")
    (let ((*destination* 'TRASH))
      (c2expr* body))
    (wt-nl "ecl_stack_pop_values(cl_env_copy," nargs ");")
    ;; Finally, if the protected form was aborted, jump to the
    ;; next catch point...
    (wt-nl "if (unwinding) ecl_unwind(cl_env_copy,next_fr);")
    ;; ... or simply return the values of the protected form.
    (unwind-exit 'VALUES)
    (wt-nl-close-brace)))

(defun c1throw (args)
  (check-args-number 'THROW args 2 2)
  (make-c1form* 'THROW :args (c1expr (first args)) (c1expr (second args))))

(defun c2throw (c1form tag val &aux loc)
  (declare (ignore c1form))
  (case (c1form-name tag)
    ((VAR LOCATION) (setq loc (c1form-arg 0 tag)))
    (t (setq loc (make-temp-var))
       (let ((*destination* loc)) (c2expr* tag))))
  (let ((*destination* 'VALUES)) (c2expr* val))
  (wt-nl "cl_throw(" loc ");"))
