;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPTAG  --  Tagbody and Go.

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

;;;  A dummy variable is created to hold the tag identifier and one tag
;;;  structure (containing reference to such variable) is created for each
;;;  label in the body.
;;;  When a reference to a tag (go instruction) is found, the
;;;  var-kind is stepped from NIL to OBJECT (if appearing inside an
;;;  unwind-protect) to LEXICAL or CLOSURE (if appearing across a boundary).
;;;  The tag-ref is also incremented.
;;;  Therefore var-ref represents whether some tag is used at all and var-kind
;;;  variable represents whether a tag identifier must be created and the
;;;  kind of the dummy variable to store it.


(defvar *reg-amount* 60)
;;; amount to increase var-ref for each variable reference inside a loop

(defun add-loop-registers (tagbody)
  ;; Find a maximal iteration interval in TAGBODY from first to end
  ;; then increment the var-ref slot.
  (labels ((add-reg1 (form)
	     ;; increase the var-ref in FORM for all vars
	     (cond ((c1form-p form)
		    (dolist (v (c1form-args form))
		      (add-reg1 v)))
		   ((consp form)
		    (dolist (v form)
		      (add-reg1 v)))
		   ((var-p form)
		    (setf (var-ref form) most-positive-fixnum))))
	   (jumps-to-p (clause tag-name)
	     ;; Does CLAUSE have a go TAG-NAME in it?
	     (cond ((c1form-p clause)
		    (and (eq (c1form-name clause) 'GO)
			 (eq (tag-name (c1form-arg 0 clause)) tag-name)))
		   ((atom clause) nil)
		   (t (or (jumps-to-p (car clause) tag-name)
			  (jumps-to-p (cdr clause) tag-name))))))
    (do ((v tagbody (cdr v))
	 (end nil)
	 (first nil))
	((null v)
	 (do ((ww first (cdr ww)))
	     ((eq ww end) (add-reg1 (car ww)))
	   (add-reg1 (car ww))))
      (when (tag-p (car v))
	(unless first (setq first v))
	(do ((w (cdr v) (cdr w))
	     (name (tag-name (car v))))
	    ((null w))
	  (when (jumps-to-p (car w) name)
	    (setq end w)))))))

;; FIXME! The variable name should not be a usable one!
(defun c1tagbody (orig-body &aux (*cmp-env* (cmp-env-copy))
		  (tag-var (make-var :name 'TAGBODY :kind NIL))
		  (tag-index 0)
		  (body nil))

  ;;; Establish tags.
  (setq body
	(loop for x in orig-body
	   collect (if (consp x)
		       x
		       (let ((tag (make-tag :name x :var tag-var :index tag-index)))
			 (cmp-env-register-tag (tag-name tag) tag)
			 (incf tag-index)
			 tag))))
  ;; Split forms according to the tag they are preceded by and compile
  ;; them grouped by PROGN. This help us use the optimizations in
  ;; C1PROGN to recognize transfers of control.
  (loop for form in body
     with output = '()
     with tag-body = nil
     with this-tag = (make-var :name 'tagbody-beginnnig :kind nil)
     do (cond ((tag-p form)
	       (when tag-body
		 (setf output (cons (c1progn (nreconc tag-body '(nil))) output)
		       tag-body nil))
	       (push form output))
	      (t
	       (push form tag-body)))
     finally (setf body
		   (if tag-body
		       (cons (c1progn (nreconc tag-body '(nil))) output)
		       output)))

  ;;; Reverse the body list, deleting unused tags.
  (loop for form in body
     with output = '()
     when (or (not (tag-p form)) (plusp (tag-ref form)))
     do (push form output)
     finally (setf body output))

  ;;; Ensure that the end is not just a tag, but at least a NIL body.
  (when (null body)
    (return-from c1tagbody (c1progn nil)))
  (when (tag-p (first (last body)))
    (setf body (nconc body (list (c1expr nil)))))

  ;;; Only produce a tagbody if it was needed.
  (when (zerop (var-ref tag-var))
    (return-from c1tagbody (make-c1form* 'PROGN :args
					 (delete-if #'tag-p body))))
  (when (var-ref-ccb tag-var)
    (incf *setjmps*))
  (add-loop-registers body)
  (make-c1form* 'TAGBODY :local-vars (list tag-var)
		:args tag-var body))

(defun c2tagbody (c1form tag-loc body)
  (declare (type var tag-loc)
	   (ignore c1form))
  (if (null (var-kind tag-loc))
      ;; only local goto's
      (dolist (x body (c2tagbody-body body))
	;; Allocate labels.
	(when (and (tag-p x) (plusp (tag-ref x)))
	  (setf (tag-label x) (next-label*))
	  (setf (tag-unwind-exit x) *unwind-exit*)))
      ;; some tag used non locally or inside an unwind-protect
      (let ((*unwind-exit* (cons 'FRAME *unwind-exit*))
	    (*env* *env*) (*env-lvl* *env-lvl*)
	    (*lex* *lex*) (*lcl* *lcl*)
	    (*inline-blocks* 0)
	    (env-grows (env-grows (var-ref-ccb tag-loc))))
	(when env-grows
	  (let ((env-lvl *env-lvl*))
	    (maybe-open-inline-block)
	    (wt-nl "volatile cl_object env" (incf *env-lvl*)
		   " = env" env-lvl ";")))
	(when (eq :OBJECT (var-kind tag-loc))
	  (setf (var-loc tag-loc) (next-lcl))
	  (maybe-open-inline-block)
	  (wt-nl "cl_object " tag-loc ";")
	  (setq env-grows t))		; just to ensure closing the block
	(bind "ECL_NEW_FRAME_ID(cl_env_copy)" tag-loc)
	(wt-nl "if (ecl_frs_push(cl_env_copy," tag-loc ")) {")
	;; Allocate labels.
	(dolist (tag body)
	  (when (and (tag-p tag) (plusp (tag-ref tag)))
	    (setf (tag-label tag) (next-label))
	    (setf (tag-unwind-exit tag) *unwind-exit*)
	    (wt-nl "if (cl_env_copy->values[0]==ecl_make_fixnum(" (tag-index tag) "))")
	    (wt-go (tag-label tag))))
	(when (var-ref-ccb tag-loc)
	  (wt-nl "ecl_internal_error(\"GO found an inexistent tag\");"))
	(wt-nl "}")
	(c2tagbody-body body)
	(close-inline-blocks))))

(defun c2tagbody-body (body)
  ;;; INV: BODY is a list of tags and forms. We have processed the body
  ;;; so that the last element is always a form producing NIL.
  (do ((l body (cdr l)))
      ((null l))
    (let* ((this-form (first l)))
      (cond ((tag-p this-form)
	     (wt-label (tag-label this-form)))
	    ((endp (rest l))
	     ;; Last form, it is never a label!
	     (c2expr this-form))
	    (t
	     (let* ((next-form (second l))
		    (*exit* (if (tag-p next-form)
				(tag-label next-form)
				(next-label)))
		    (*unwind-exit* (cons *exit* *unwind-exit*))
		    (*destination* 'TRASH))
	       (c2expr this-form)
	       (unless (tag-p next-form)
		 (wt-label *exit*))))))))

(defun c1go (args)
  (check-args-number 'GO args 1 1)
  (let ((name (first args)))
    (unless (or (symbolp name) (integerp name))
      (cmperr "The tag name ~s is not a symbol nor an integer." name))
    (multiple-value-bind (tag ccb clb unw)
	(cmp-env-search-tag name)
      (unless tag
	(cmperr "Undefined tag ~A" name))
      (let ((var (tag-var tag)))
	(cond (ccb (setf (tag-ref-ccb tag) t
			 (var-ref-ccb var) T
			 (var-kind var) 'CLOSURE))
	      (clb (setf (tag-ref-clb tag) t
			 (var-ref-clb var) t
			 (var-kind var) 'LEXICAL))
	      (unw (unless (var-kind var)
		     (setf (var-kind var) :OBJECT))))
	(incf (tag-ref tag))
	(add-to-read-nodes var (make-c1form* 'GO :args tag (or ccb clb unw)))))))

(defun c2go (c1form tag nonlocal)
  (declare (ignore c1form))
  (if nonlocal
      (let ((var (tag-var tag)))
	(wt-nl "cl_go(" var ",ecl_make_fixnum(" (tag-index tag) "));"))
      ;; local go
      (progn
	(unwind-no-exit-until (tag-unwind-exit tag))
	(wt-nl) (wt-go (tag-label tag)))))
