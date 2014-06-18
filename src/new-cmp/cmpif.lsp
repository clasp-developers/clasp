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

;;;; CMPIF  Conditionals.

(in-package "COMPILER")

(defun c1alternatives (form true-branch false-branch)
  (c1with-saved-value (prefix postfix temp form)
    (when (or prefix postfix)
      (setf (var-kind temp) :bool))
    (nconc prefix
           (if true-branch
               (nconc (c1jmp-true true-branch temp)
                      (if false-branch (c1jmp false-branch)))
               (c1jmp-false false-branch temp))
           postfix)))

(defun c1if-and (forms true-branch false-branch)
  (cond ((null forms)
         t)
        ((null (rest forms))
         (c1condition (first forms) true-branch false-branch))
        ((null false-branch)
         (setf false-branch (make-tag :name (gensym "AND-FALSE") :label (next-label)))
         (let ((f (c1if-and forms true-branch false-branch)))
           (if (atom f)
               f
               (nconc f (list false-branch)))))
        (t
         (loop with output = '()
            for f on forms
            for form = (first f)
            do (let ((x (c1condition form
                                     (if (rest f) nil true-branch)
                                     false-branch)))
                 (cond ((eq x :always-false)
                        (return (nreconc output (c1jmp false-branch))))
                       ((eq x :always-true)
                        ;; True branch, we do nothing
                        )
                       (t
                        (setf output (nreconc x output)))))
            finally (return (nreverse output))))))

(defun c1if-or (forms true-branch false-branch)
  (cond ((null forms)
         nil)
        ((null (rest forms))
         (c1condition (first forms) true-branch false-branch))
        ((null true-branch)
         (setf true-branch (make-tag :name (gensym "OR-TRUE") :label (next-label)))
         (let ((f (c1if-or forms true-branch false-branch)))
           (if (atom f)
               f
               (nconc f (list true-branch)))))
        (t
         (loop with output = '()
            for f on forms
            for form = (first f)
            do (let ((x (c1condition form
                                     true-branch
                                     (if (rest f) nil false-branch))))
                 (cond ((eq x :always-false)
                        ;; Always false, we do nothing
                        )
                       ((eq x :always-true)
                        (return (nreconc output (c1jmp true-branch))))
                       (t
                        (setf output (nreconc x output)))))
            finally (return (nreverse output))))))

(defun c1condition (form true-branch false-branch)
  (cond ((constantp form)
         (if (cmp-eval form) :always-true :always-false))
        ((atom form)
         (c1alternatives form true-branch false-branch))
        (t
         (case (first form)
           (AND (c1if-and (rest form) true-branch false-branch))
           (OR (c1if-or (rest form) true-branch false-branch))
           (NOT (check-args-number 'NOT (rest form) 1 1)
                (let ((f (c1condition (second form) false-branch true-branch)))
                  (case f
                    ((:always-true) :always-false)
                    ((:always-false) :always-true)
                    (otherwise f))))
           (otherwise (c1alternatives form true-branch false-branch))))))

#+nil
(defun c1if (destination args)
  (check-args-number 'IF args 2 3)
  (if (and (eq destination 'TRASH) (= (length args) 2))
      (let* ((tag-false (make-tag :name (gensym "WHEN-EXIT") :label (next-label)))
             (true-branch (second args))
             (condition (first args))
             (f (c1condition condition nil tag-false)))
        (case f
          ((:always-true) (c1translate destination true-branch))
          ((:always-false) (c1translate destination nil))
          (otherwise
           (nconc f
                  (c1translate destination true-branch)
                  (list tag-false)))))
      (let* ((tag-true (make-tag :name (gensym "IF-TRUE") :label (next-label)))
             (tag-exit (make-tag :name (gensym "IF-EXIT") :label (next-label)))
             (false-branch (third args))
             (true-branch (second args))
             (condition (first args))
             (f (c1condition condition tag-true nil)))
        (case f
          ((:always-false) (c1translate destination false-branch))
          ((:always-true) (c1translate destination true-branch))
          (t (nconc f
                    (c1translate destination false-branch)
                    (c1jmp tag-exit)
                    (list tag-true)
                    (c1translate destination true-branch)
                    (list tag-exit)))))))

(defun c1if (destination args)
  (check-args-number 'IF args 2 3)
  (let* ((tag-true (make-tag :name (gensym "IF-TRUE") :label (next-label)))
         (tag-exit (make-tag :name (gensym "IF-EXIT") :label (next-label)))
         (false-branch (third args))
         (true-branch (second args))
         (condition (first args)))
    (nconc (c1alternatives condition tag-true nil)
           (c1translate destination false-branch)
           (c1jmp tag-exit)
           (list tag-true)
           (c1translate destination true-branch)
           (list tag-exit))))
