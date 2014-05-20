;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;; **********************************************************************

(in-package "SI")

(eval-when (:compile-toplevel :execute)

#+ecl-min
(defmacro handler-bind (bindings &body body)
  `(progn ,@body))

;;;; The Once-Only macro:

;;; Once-Only  --  Interface
;;;
;;;    Once-Only is a utility useful in writing source transforms and macros.
;;; It provides an easy way to wrap a let around some code to ensure that some
;;; forms are only evaluated once.
;;;
(defmacro once-only (specs &body body)
  "Once-Only ({(Var Value-Expression)}*) Form*
  Create a Let* which evaluates each Value-Expression, binding a temporary
  variable to the result, and wrapping the Let* around the result of the
  evaluation of Body.  Within the body, each Var is bound to the corresponding
  temporary variable."
  (labels ((frob (specs body)
	     (if (null specs)
		 `(progn ,@body)
		 (let ((spec (first specs)))
		   (when (/= (length spec) 2)
		     (error "Malformed Once-Only binding spec: ~S." spec))
		   (let ((name (first spec))
			 (exp-temp (gensym)))
		     `(let ((,exp-temp ,(second spec))
			    (,name (gensym "OO-")))
		       `(let ((,,name ,,exp-temp))
			 ,,(frob (rest specs) body))))))))
    (frob specs body)))

;;;; The Collect macro:

;;; Collect-Normal-Expander  --  Internal
;;;
;;;    This function does the real work of macroexpansion for normal collection
;;; macros.  N-Value is the name of the variable which holds the current
;;; value.  Fun is the function which does collection.  Forms is the list of
;;; forms whose values we are supposed to collect.
;;;
(defun collect-normal-expander (n-value fun forms)
  `(progn
    ,@(mapcar #'(lambda (form) `(setq ,n-value (,fun ,form ,n-value))) forms)
    ,n-value))

;;; Collect-List-Expander  --  Internal
;;;
;;;    This function deals with the list collection case.  N-Tail is the pointer
;;; to the current tail of the list, which is NIL if the list is empty.
;;;
(defun collect-list-expander (n-value n-tail forms)
  (let ((n-res (gensym)))
    `(progn
      ,@(mapcar #'(lambda (form)
		    `(let ((,n-res (cons ,form nil)))
		       (cond (,n-tail
			      (setf (cdr ,n-tail) ,n-res)
			      (setq ,n-tail ,n-res))
			     (t
			      (setq ,n-tail ,n-res  ,n-value ,n-res)))))
		forms)
      ,n-value)))


;;; Collect  --  Public
;;;
;;;    The ultimate collection macro...
;;;
(defmacro collect (collections &body body)
  "Collect ({(Name [Initial-Value] [Function])}*) {Form}*
  Collect some values somehow.  Each of the collections specifies a bunch of
  things which collected during the evaluation of the body of the form.  The
  name of the collection is used to define a local macro, a la MACROLET.
  Within the body, this macro will evaluate each of its arguments and collect
  the result, returning the current value after the collection is done.  The
  body is evaluated as a PROGN; to get the final values when you are done, just
  call the collection macro with no arguments.

  Initial-Value is the value that the collection starts out with, which
  defaults to NIL.  Function is the function which does the collection.  It is
  a function which will accept two arguments: the value to be collected and the
  current collection.  The result of the function is made the new value for the
  collection.  As a totally magical special-case, the Function may be Collect,
  which tells us to build a list in forward order; this is the default.  If an
  Initial-Value is supplied for Collect, the stuff will be rplacd'd onto the
  end.  Note that Function may be anything that can appear in the functional
  position, including macros and lambdas."

  (let ((macros ())
	(binds ()))
    (dolist (spec collections)
      (unless (<= 1 (length spec) 3)
	(error "Malformed collection specifier: ~S." spec))
      (let ((n-value (gensym))
	    (name (first spec))
	    (default (second spec))
	    (kind (or (third spec) 'collect)))
	(push `(,n-value ,default) binds)
	(if (eq kind 'collect)
	    (let ((n-tail (gensym)))
	      (if default
		  (push `(,n-tail (last ,n-value)) binds)
		  (push n-tail binds))
	      (push `(,name (&rest args)
			    (collect-list-expander ',n-value ',n-tail args))
		    macros))
	    (push `(,name (&rest args)
			  (collect-normal-expander ',n-value ',kind args))
		  macros))))
    `(macrolet ,macros (let* ,(nreverse binds) ,@body))))

); eval-when

;;; Automate an idiom often found in macros:
;;;   (LET ((FOO (GENSYM "FOO"))
;;;         (MAX-INDEX (GENSYM "MAX-INDEX-")))
;;;     ...)
;;;
;;; "Good notation eliminates thought." -- Eric Siggia
;;;
;;; Incidentally, this is essentially the same operator which
;;; _On Lisp_ calls WITH-GENSYMS.
(defmacro with-unique-names (symbols &body body)
  `(let* ,(mapcar (lambda (symbol)
                    (let* ((symbol-name (symbol-name symbol))
                           #+ecl-min
                           (stem symbol-name)
                           #-ecl-min
                           (stem (if (every #'alpha-char-p symbol-name)
                                     nil
                                     symbol-name
                                     (concatenate 'string symbol-name "-"))))
                      `(,symbol (gensym ,stem))))
                  symbols)
     ,@body))

(defmacro with-clean-symbols (symbols &body body)
  "Rewrites the given forms replacing the given symbols with uninterned
ones, which is useful for creating hygienic macros."
  `(progn ,@(sublis (mapcar #'(lambda (s) (cons s (make-symbol (symbol-name s))))
			    symbols)
		    body)))
