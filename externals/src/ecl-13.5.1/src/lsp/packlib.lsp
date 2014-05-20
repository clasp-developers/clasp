;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;                    package routines

(in-package "SYSTEM")

(defun find-all-symbols (string-or-symbol)
  "Args: (string-designator)
Returns a list of all symbols that have the specified print name.
STRING-DESIGNATOR may be a symbol, in which case the print name of the symbol
is used."
  (let ((symbol-name (string string-or-symbol)))
    (mapcan #'(lambda (p)
		(multiple-value-bind (s i)
		    (find-symbol symbol-name p)
		  (if (or (eq i :internal) (eq i :external))
		      (list s)
		      nil)))
	    (list-all-packages))))

(defun packages-iterator (packages options maybe-list)
  (let ((all-symbols nil))
    (when (or (atom packages) (not maybe-list))
      (setq packages (list packages)))
    (dolist (p packages)
      (let ((package (si::coerce-to-package p)))
	(multiple-value-bind (hash-ext hash-int packages-used)
	    (si::package-hash-tables package)
	  (when (member :external options)
	    (push (list package :external hash-ext) all-symbols))
	  (when (member :internal options)
	    (push (list package :internal hash-int) all-symbols))
	  (when (member :inherited options)
	    (dolist (p packages-used)
	      (push (list package :inherited (si::package-hash-tables p))
		    all-symbols))))))
    (unless all-symbols
      (return-from packages-iterator #'(lambda () (values nil nil nil nil))))
    (let* ((current (pop all-symbols))
	   (package (first current))
	   (type (second current))
	   (iterator (si::hash-table-iterator (third current))))
      (flet ((iterate ()
	       (tagbody
		AGAIN
		 (multiple-value-bind (found key value)
		     (funcall iterator)
		   (declare (ignore key))
		   (cond 
		     (found
		      (when (eq type :inherited)
			(multiple-value-bind (s access)
			    (find-symbol (symbol-name value) package)
			  (unless (and (eq s value) (eq access type))
			    (go AGAIN))))
		      (return-from iterate (values t value type package)))
		     ((null all-symbols)
		      (return-from iterate (values nil nil nil nil)))
		     (t
		      (setq current (pop all-symbols))
		      (setq package (first current)
			    type (second current)
			    iterator (si::hash-table-iterator (third current))
			    ))))
		  (go AGAIN))))
	#'iterate))))

(defmacro with-package-iterator ((iterator package-list &rest conditions)
				 &rest body)
  (if conditions
      (let ((aux (set-difference conditions '(:external :internal :inherited))))
	(when aux
	  (signal-simple-error 'program-error nil "Clauses ~{~S~} are not allowed."
			       (list aux))))
      (signal-simple-error 'program-error
			   nil
			   "Must supply at least one of :inherited, :external or :internal"
			   nil))
  `(let ((,iterator (packages-iterator ,package-list ',conditions t)))
    (macrolet ((,iterator () (list 'funcall ',iterator)))
      ,@body)))

(defun expand-do-symbols (var package result-form body options)
  (declare (si::c-local))
  (let* ((i (gensym))
	 (found (gensym))
	 declaration doc)
    (multiple-value-setq (declaration body doc)
      (find-declarations body nil))
    `(do* ((,i (packages-iterator ,package ',options t))
	   ,found ,var)
	  (nil)
	,@declaration
	(multiple-value-setq (,found ,var) (funcall ,i))
	(unless ,found (return ,result-form))
	,@body)))

(defmacro do-symbols ((var &optional (package '*package*) (result-form nil))
                      &rest body)
  "Syntax: (do-symbols (var [package [result]])
          {decl}* {tag | statement}*)
Executes STATEMENTs once for each symbol in PACKAGE (which defaults to the
current package), with VAR bound to the symbol.  Then evaluates RESULT (which
defaults to NIL) and returns all values."
  (expand-do-symbols var package result-form body '(:inherited :internal :external)))

(defmacro do-external-symbols
          ((var &optional (package '*package*) (result-form nil)) &rest body)
  "Syntax: (do-external-symbols (var [package [result]])
          {decl}* {tag | statement}*)
Establishes a NIL block and executes STATEMENTs once for each external symbol
in PACKAGE (which defaults to the current package), with VAR bound to the
variable.  Then evaluates RESULT (which defaults to NIL) and returns all
values."
  (expand-do-symbols var package result-form body '(:external)))

(defmacro do-all-symbols ((var &optional (result-form nil)) &rest body)
  "Syntax: (do-all-symbols (var [result]) {decl}* {tag | statement}*)
Establishes a NIL block and executes STATEMENTs once for each symbol in each
package, with VAR bound to the symbol.  Then evaluates RESULT (which defaults
to NIL) and returns all values."
  (expand-do-symbols var '(list-all-packages) result-form body '(:internal :external)))

(defun print-symbol-apropos (symbol)
  (prin1 symbol)
  (when (fboundp symbol)
        (if (special-operator-p symbol)
            (princ "  Special form")
            (if (macro-function symbol)
                (princ "  Macro")
                (princ "  Function"))))
  (when (boundp symbol)
        (if (constantp symbol)
            (princ "  Constant: ")
            (princ "  has value: "))
        (prin1 (symbol-value symbol)))
  (terpri))


(defun apropos (string &optional package)
  "Args: (string &optional (package nil))
Prints those symbols whose print-names contain STRING as substring.  If
PACKAGE is non-NIL, then only the specified PACKAGE is searched."
  (setq string (string string))
  (mapc #'print-symbol-apropos (apropos-list string package))
  (values))

(defun apropos-list (string &optional package)
  "Args: (string &optional (package nil))
Returns a list of all symbols whose print-names contain STRING as substring.
If PACKAGE is non-NIL, then only the specified PACKAGE is searched."
  (sort (delete-duplicates (apropos-list-inner string package))
	#'(lambda (s1 s2)
	    (string-lessp (prin1-to-string s1)
			  (prin1-to-string s2)))))

(defun apropos-list-inner (string package)
  (declare (si::c-local))
  (let* ((list '())
	 (string (string string)))
    (cond (package
	   (dolist (p (package-use-list package))
	     (setf list (nconc (apropos-list-inner string p) list)))
	   (do-symbols (symbol package)
	     (when (search string (string symbol) :test #'char-equal)
	       (setq list (cons symbol list)))))
	  (t
	   (do-all-symbols (symbol)
	     (when (search string (string symbol) :test #'char-equal)
	       (setq list (cons symbol list))))))
    list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HIERARCHICAL PACKAGE NAMES
;;
;; Code provided by Franz Inc. to the public domain and adapted for ECL.
;;

(defun find-relative-package (name)
  ;; Given a package name, a string, do a relative package name lookup.
  ;;
  (declare (optimize speed))
  (flet ((relative-to (package name)
	   (if (zerop (length name))
	       package
	       (find-package (concatenate 'simple-string (package-name package) "." name))))
	 (find-non-dot (name)
	   (do* ((len (length name))
		 (i 0 (1+ i)))
	       ((= i len) nil)
	     (declare (fixnum len i))
	     (when (char/= #\. (char name i)) (return i)))))
    (when (and (stringp name)
               (plusp (length name))
               (char= #\. (char name 0)))
      (let* ((last-dot-position (or (find-non-dot name) (length name)))
	     (n-dots (truly-the fixnum last-dot-position))
	     (name (subseq name last-dot-position)))
	;; relative to our (- n-dots 1)'th parent
	(let ((p *package*))
	  (dotimes (i (1- n-dots))
	    (declare (fixnum i))
	    (let ((tmp (package-parent p)))
	      (unless tmp
		(error "The parent of ~a does not exist." p))
	      (setq p tmp)))
	  (relative-to p name))))))

(defun package-parent (package-specifier)
  ;; Given package-specifier, a package, symbol or string, return the
  ;; parent package.  If there is not a parent, signal an error.
  ;;
  ;; Because this function is called via the reader, we want it to be as
  ;; fast as possible.
  (declare (optimize speed))
  (flet ((find-last-dot (name)
	   (do* ((len (1- (length name)))
		 (i len (1- i)))
	       ((= i -1) nil)
	     (declare (fixnum len i))
	     (when (char= #\. (char name i)) (return i)))))
    (let* ((child (cond ((packagep package-specifier)
			 (package-name package-specifier))
			((symbolp package-specifier)
			 (symbol-name package-specifier))
			((stringp package-specifier) package-specifier)
			(t (error "Illegal package specifier: ~s."
				  package-specifier))))
	   (dot-position (find-last-dot child)))
      (if dot-position
	  (let ((parent (subseq child 0 dot-position)))
	    (or (find-package parent)
		(error "The parent of ~a does not exist." child))))
	  (error "There is no parent of ~a." child))))

(defun package-children (package-specifier &key (recurse t))
  ;; Given package-specifier, a package, symbol or string, return all the
  ;; packages which are in the hierarchy "under" the given package.  If
  ;; :recurse is nil, then only return the immediate children of the
  ;; package.
  ;;
  ;; While this function is not called via the reader, we do want it to be
  ;; fast.
  (declare (optimize speed))
  (let* ((res ())
         (parent (cond ((packagep package-specifier)
                        (package-name package-specifier))
		       ((symbolp package-specifier)
			(symbol-name package-specifier))
		       ((stringp package-specifier) package-specifier)
		       (t (error "Illegal package specifier: ~s." package-specifier))))
	 (parent-prefix (concatenate 'simple-string parent ".")))
    (labels
	((string-prefix-p (prefix string)
	   ;; Return length of `prefix' if `string' starts with `prefix'.
	   ;; We don't use `search' because it does much more than we need
	   ;; and this version is about 10x faster than calling `search'.
	   (let ((prefix-len (length prefix))
		 (seq-len (length string)))
	     (declare (fixnum prefix-len seq-len))
	     (when (>= prefix-len seq-len)
	       (return-from string-prefix-p nil))
	     (do* ((i 0 (1+ i)))
		 ((= i prefix-len) prefix-len)
	       (declare (fixnum i))
	       (when (not (char= (char prefix i) (char string i)))
		 (return nil))))))
      (dolist (package (list-all-packages))
	(let* ((package-name (package-name package))
	       (prefix (string-prefix-p parent-prefix package-name)))
	  (when (and prefix (or recurse (not (find #\. package-name :start prefix))))
	    (pushnew package res)))))))
