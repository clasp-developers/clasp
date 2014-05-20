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
		    (incf (var-ref form) (the fixnum *reg-amount*)))))
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

(defun make-tagbody-labels (body *cmp-env*)
  "Produces two values. The first one is a list of forms where atoms have been
replaced with tags, collapsing pairs of consecutive atoms into the same tag.
The second value is an association list of atoms to the tags they represent."
  (let ((tags '()))
    (values (loop with tag-index = 0
               with last-tag = nil
               with tag-env = *cmp-env*
               for form in body
               unless (when (and last-tag (atom form))
                        (cmp-env-register-tag form last-tag)
                        (push last-tag tags)
                        t)
               collect (if (atom form)
                           (let ((tag (make-tag :name form :index tag-index
                                                :label (next-label)
                                                :env *cmp-env*)))
                             (cmp-env-register-tag form tag)
                             (push tag tags)
                             (incf tag-index)
                             (setf last-tag tag))
                           (progn
                             (setf last-tag nil)
                             form)))
            tags
            *cmp-env*)))

(defun c1tagbody (destination orig-body &aux (*cmp-env* *cmp-env*)
		  (tag-var (make-var :name (gensym "TAGBODY-ID") :kind NIL))
		  (tag-index 0)
		  (body nil)
                  (tags nil))
  ;; Register variable and frame for cleanup forms
  (cmp-env-register-var tag-var *cmp-env*)
  ;(cmp-env-register-frs tag-var *cmp-env*)

  ;; Establish tags.
  (multiple-value-setq (body tags *cmp-env*) (make-tagbody-labels orig-body *cmp-env*))

  ;; Ensure that the end is not just a tag, but at least a NIL body.
  (when (every #'tag-p body)
    (return-from c1tagbody (c1nil destination)))

  ;; Assign each tag the tagbody variable so that GO can find it.
  (loop for tag in tags
     do (setf (tag-var tag) tag-var))

  ;; Split forms according to the tag they are preceded by and compile
  ;; them grouped by PROGN. This help us use the optimizations in
  ;; C1PROGN to recognize transfers of control.
  (loop for form in body
     with output = '()
     with tag-body = nil
     do (cond ((tag-p form)
	       (when tag-body
		 (setf output (cons (nreverse tag-body) output)
		       tag-body nil))
	       (push form output))
	      (t
	       (push form tag-body)))
     finally (setf body (if tag-body
                            (cons (nreverse tag-body) output)
                            output)))

  ;; Compile the grouped forms, in order. All values are discarded
  ;; and we add a final NIL form.
  (setf body (loop for form in (nreverse body)
                nconc (if (tag-p form)
                          (list form)
                          (c1progn 'TRASH form))))

  ;; Delete unused tags.
  (setf body (delete-if #'(lambda (x) (and (tag-p x) (zerop (tag-ref x))))
                        body))

  ;; When the variable of the tag forms is not referenced, we can just
  ;; output the list of forms and tags.
  (when (zerop (var-ref tag-var))
    (return-from c1tagbody (nconc body (c1nil destination))))

  (add-loop-registers body)
  (let ((normal-tag (make-tag :name "TAGBODY-NORMAL" :label (next-label))))
    (nconc (c1bind (list tag-var))
           (c1frame-id tag-var)
           (c1frame-set tag-var normal-tag)
           (loop for tag in tags
              when (or (tag-ref-ccb tag) (tag-ref-clb tag))
              collect (c1translate `(JMP-TRUE ,tag)
                                   `(EQ (VALUES-REF 0) ,(tag-index tag))))
           (c1translate 'TRASH '(error "Unknown GO tag"))
           (list normal-tag)
           body
           (c1frame-pop tag-var)
           (c1unbind (list tag-var))
           (c1translate destination nil))))

(defun c1go (destination args)
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
                         (var-ref-clb var) t))
              (unw (setf (tag-ref-clb tag) t)
                   (unless (var-kind var)
                     (setf (var-kind var) :OBJECT))))
        (incf (tag-ref tag))
        (if (or ccb clb unw)
            (add-to-read-nodes var (c1go-op tag))
            (c1jmp tag))))))
