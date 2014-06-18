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

;;;; CMPBLOCK  Block and Return-from.

(in-package "COMPILER")

;;; A dummy variable is created to hold the block identifier.  When a
;;; reference to the block (via return-from) is found, the var-ref
;;; count for that variable is incremented only if the reference
;;; appears across a boundary (CB, LB or UNWIND-PROTECT), while the
;;; blk-ref is always incremented.  Therefore blk-ref represents
;;; whether the block is used at all and var-ref for the dummy
;;; variable represents whether a block identifier must be created and
;;; stored in such variable.

(defun c1block (destination args)
  (check-args-number 'BLOCK args 1)
  (let ((block-name (first args)))
    (unless (symbolp block-name)
      (cmperr "The block name ~s is not a symbol." block-name))
    (let* ((blk-var (make-var :name (gensym (symbol-name block-name)) :kind 'LEXICAL))
	   (cleanup-form (c1frame-pop blk-var))
           (*cmp-env* (cmp-env-copy *cmp-env*))
           (exit (make-tag :name (gensym "BLOCK") :label (next-label)))
	   (blk (make-blk :var blk-var :name block-name :destination destination
                          :exit exit)))
      (cmp-env-register-var blk-var *cmp-env*)
      (cmp-env-register-block blk *cmp-env*)
      (cmp-env-register-tag (tag-name exit) exit *cmp-env*)
      (cmp-env-register-cleanup cleanup-form *cmp-env*)
      (setf (blk-env blk) *cmp-env*)
      (let ((body (c1translate destination `(progn ,@(rest args)))))
        (setf (blk-destination blk) destination)
        (if (plusp (var-ref blk-var))
            (nconc (c1bind (list blk-var))
                   (c1frame-id blk-var)
                   (c1frame-set blk-var exit)
                   (c1set-from-values destination)
                   (c1jmp exit)
                   body
                   (list exit)
                   (c1unbind (list blk-var))
                   cleanup-form)
            (nconc body
                   (list exit)))))))

(defun c1return-from (destination args)
  (check-args-number 'RETURN-FROM args 1 2)
  (let ((name (first args)))
    (unless (symbolp name)
      (cmperr "The block name ~s is not a symbol." name))
    (multiple-value-bind (blk ccb clb unw)
	(cmp-env-search-block name)
      (unless blk
	(cmperr "The block ~s is undefined." name))
      (let* ((destination (blk-destination blk))
	     (var (blk-var blk))
	     output)
	(cond (ccb (setf (blk-ref-ccb blk) t
                         (var-kind var) 'CLOSURE
			 (var-ref-ccb var) T))
	      (clb (setf (blk-ref-clb blk) t
                         (var-ref-clb var) t
                         (var-kind var) 'LEXICAL))
	      (unw (setf type 'UNWIND-PROTECT)
                   (unless (var-kind var)
                     (setf (var-kind var) :OBJECT))))
        (if (or ccb clb unw)
            (let* ((val (c1translate 'VALUES (second args)))
                   (return-stmt (c1return-from-op var (blk-name blk))))
              (setf output (nconc val (c1cleanup-forms (blk-env blk)) return-stmt))
              (add-to-read-nodes var return-stmt))
            (c1with-saved-output (prefix postfix new-destination (blk-destination blk))
              (let* ((val (c1translate new-destination (second args)))
                     (cleanup (c1cleanup-forms (blk-env blk)))
                     (exit-tag (blk-exit blk)))
                (setf output (nconc prefix
                                    val
                                    cleanup
                                    postfix
                                    (c1jmp exit-tag))))))
	(incf (blk-ref blk))
        output))))
