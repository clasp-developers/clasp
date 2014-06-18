;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1995, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;                        list manipulating routines

(in-package "SYSTEM")

(defun union (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Returns, as a list, the union of elements in LIST1 and in LIST2."
  (do ((x list1 (cdr x))
       (first) (last))
      ((null x)
       (when last (rplacd last list2))
       (or first list2))
    (unless (member1 (car x) list2 test test-not key)
      (if last
	  (progn (rplacd last (cons (car x) nil))
		 (setq last (cdr last)))
	  (progn (setq first (cons (car x) nil))
		 (setq last first))))))

(defun nunion (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Destructive UNION.  Both LIST1 and LIST2 may be destroyed."
  (do ((x list1 (cdr x))
       (first) (last))
      ((null x)
       (when last (rplacd last list2))
       (or first list2))
    (unless (member1 (car x) list2 test test-not key)
      (if last
	  (rplacd last x)
	  (setq first x))
      (setq last x))))

(defun intersection (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Returns a list consisting of those objects that are elements of both LIST1 and
LIST2."
  (do ((x list1 (cdr x))
       (ans))
      ((null x)
       (nreverse ans)) ; optional nreverse: not required by CLtL
    (when (member1 (car x) list2 test test-not key)
        (push (car x) ans))))

(defun nintersection (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Destructive INTERSECTION.  Only LIST1 may be destroyed."
  (do ((x list1 (cdr x))
       (first) (last))
      ((null x)
       (when last (rplacd last nil))
       first)
    (when (member1 (car x) list2 test test-not key)
      (if last
	  (rplacd last x)
	  (setq first x))
      (setq last x))))

(defun set-difference (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Returns, as a list, those elements of LIST1 that are not elements of LIST2."
  (do ((x list1 (cdr x))
       (ans))
      ((null x) (nreverse ans))
    (unless (member1 (car x) list2 test test-not key)
      (push (car x) ans))))

(defun nset-difference (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Destructive SET-DIFFERENCE.  Only LIST1 may be destroyed."
  (do ((x list1 (cdr x))
       (first) (last))
      ((null x)
       (when last (rplacd last nil))
       first)
    (unless (member1 (car x) list2 test test-not key)
      (if last
	  (rplacd last x)
	  (setq first x))
      (setq last x))))

(defun swap-args (f)
  (declare (si::c-local))
  (and f #'(lambda (x y) (funcall f y x))))

(defun set-exclusive-or (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Returns, as a list, those elements of LIST1 that are not elements of LIST2 and
those elements of LIST2 that are not elements of LIST1."
  (nconc (set-difference list1 list2 :test test :test-not test-not :key key)
         (set-difference list2 list1 :test (swap-args test) :test-not (swap-args test-not) :key key)))

(defun nset-exclusive-or (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Destructive SET-EXCLUSIVE-OR.  Both LIST1 and LIST2 may be destroyed."
  (nconc (set-difference list1 list2 :test test :test-not test-not :key key)
	 (nset-difference list2 list1 :test (swap-args test) :test-not (swap-args test-not) :key key)))

(defun subsetp (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Returns T if every element of LIST1 is also an element of LIST2.  Returns NIL
otherwise."
  (do ((l list1 (cdr l)))
      ((null l) t)
    (unless (member1 (car l) list2 test test-not key)
      (return nil))))

(defun rassoc-if (test alist &key key)
  "Returns the first pair in ALIST whose cdr satisfies TEST. Returns NIL if no
such pair exists."
  (rassoc test alist :test #'funcall :key key))
(defun rassoc-if-not (test alist &key key)
  "Returns the first pair in ALIST whose cdr does not satisfy TEST.  Returns NIL
if no such pair exists."
  (rassoc test alist :test-not #'funcall :key key))

(defun assoc-if (test alist &key key)
  "Returns the first pair in ALIST whose car satisfies TEST.  Returns NIL if no
such pair exists."
  (assoc test alist :test #'funcall :key key))
(defun assoc-if-not (test alist &key key)
  "Returns the first pair in ALIST whose car does not satisfy TEST.  Returns NIL
if no such pair exists."
  (assoc test alist :test-not #'funcall :key key))

(defun member-if (test list &key key)
  "Searches LIST for an element that satisfies TEST.  If found, returns the
sublist of LIST that begins with the element.  If not found, returns NIL."
  (member test list :test #'funcall :key key))
(defun member-if-not (test list &key key)
  "Searches LIST for an element that does not satisfy TEST.  If found, returns
the sublist of LIST that begins with the element.  If not found, returns NIL."
  (member test list :test-not #'funcall :key key))

(defun subst-if (new test tree &key key)
  "Substitutes NEW for subtrees of TREE that satisfy TEST and returns the result.
The original TREE is not destroyed."
  (subst new test tree :test #'funcall :key key))
(defun subst-if-not (new test tree &key key)
  "Substitutes NEW for subtrees of TREE that do not satisfy TEST and returns the
result.  The original TREE is not destroyed."
  (subst new test tree :test-not #'funcall :key key))

(defun nsubst-if (new test tree &key key)
  "Destructive SUBST-IF. TREE may be modified."
  (nsubst new test tree :test #'funcall :key key))
(defun nsubst-if-not (new test tree &key key)
  "Destructive SUBST-IF-NOT. TREE may be modified."
  (nsubst new test tree :test-not #'funcall :key key))
