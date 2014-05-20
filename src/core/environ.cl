#+(version= 8 1)
(sys:defpatch "environ" 3
  "v1: add ensure-compiled-execution and with-compiled-body.
v2: fix package error in v1.
v3: fix walking for methods with ensure-compiled-execution forms."
  :type :system
  :post-loadable t)

;; -*- mode: common-lisp; package: system -*-
;; environ.cl
;; portable environments implementation
;;
;; copyright (c) 1985 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2007 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252 52.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $Id: environ.cl,v 1.25 2009/03/19 05:37:11 duane Exp $

#+sbcl
(defpackage :system
  (:use :common-lisp)
  (:nicknames :sys))

(in-package :system)

(eval-when (:compile-toplevel)
  (declaim (optimize speed)))


;; Current status of this module pair:
;; These statements are not warrantees of any kind.
;; 
;; Allegro CL:  This module does not load into Allegro CL 7.0, but
;; the equivalent module is already integrated and loaded into every
;; Allegro CL 7.0 lisp at level 4.  This module will be a part of
;; the next version of Allegro CL beyond 7.0.
;;
;; Clisp: Tried 2.33.2 at level 1 on FreeBSD/x86 - seems to work
;;
;; CMUCL: Tried 18e at level 1 on FreeBSD/x86 - seems to work
;;        Tried 19a at level 1 on linux - seems to work
;;
;; Lispworks: Compiled and loaded an old 4.1 version, but printing an
;; environment causes an error.
;;
;; SBCL: Tried 0.8.16 at level 1 on FreeBSD/x86 - seems to work
;;
;; Corman: Not tried
;;
;; Others:


#+(and allegro (not new-environments))
(error "This environments access module is not compatible with version 7.0 ~
of Allegro CL.  It (or its progeny) should work in later versions of Allegro CL ~
when they come out.  Users of Allegro CL 7.0 should be able to look at this ~
file as a reference, but some parts (e.g. structure ordering, variable names, ~
and names and semantics of environment kinds) have changed incompatibly.")


;; This section should house whatever code is required to allow definitions
;; to be added into the system package.  The complementary section is at the
;; end of the file.

#+cmu
(progn
  (eval-when (:compile-toplevel)
    (defparameter cl-user::.saved-p-lock. lisp::*enable-package-locked-errors*)
    (setq lisp::*enable-package-locked-errors* nil))
  (eval-when (:load-toplevel :execute)
    (defparameter cl-user::.saved-p-lock. lisp::*enable-package-locked-errors*)
    (setq lisp::*enable-package-locked-errors* nil)))

#+clisp
(progn
  (eval-when (:compile-toplevel)
    (defparameter cl-user::.saved-p-lock.
      (remove-if-not #'package-lock *system-package-list*))
    (setf (package-lock cl-user::.saved-p-lock.)  nil))
  (eval-when (:load-toplevel :execute)
    (defparameter cl-user::.saved-p-lock.
      (remove-if-not #'package-lock *system-package-list*))
    (setf (package-lock cl-user::.saved-p-lock.) nil)))

#+lispworks
(progn
  (shadow 'function-information (find-package :system))
  (shadow 'variable-information (find-package :system))
  (shadow 'declaration-information (find-package :system))
  (shadow 'augment-environment (find-package :system)))


;; Compatibility macros:
;;
;; This module defines two implementation-dependent functions called
;; get-property and put-property.  They are similar in nature
;; to get and (setf get), with the following exceptions:
;;
;; 1. If the object is not a symbol, it is treated similarly, and a
;;   mapping is made for the property indicator and value onto the
;;   object anyway, without the object necessarily having a property
;;   list slot.  It is intended that the only usage that this environment
;;   package will make of these non-symbol objects is lists, as in
;;   the setf-style function-names, or as in Allegro CL's function-spec
;;   extension.
;;
;; 2. There is no default value.  The nil value serves as an indication
;;   that the property does not exist.
;;
;; 3. (related to #2): Whenever put-property is to put a value of nil,
;;   the equivalent of remprop is done; the property is not kept.
;;
;; There are two non-Allegro-CL versions of get-property-impl and
;; put-property-impl defined at the end of this file for reference
;; and to get started.

(defmacro get-property (obj ind)
  #+allegro `(excl::get-property ,obj ,ind)
  #-allegro `(get-property-impl ,obj ,ind))

(defmacro put-property (obj value ind)
  #+allegro `(excl::put-property ,obj ,value ,ind)
  #-allegro `(put-property-impl ,obj ,value ,ind))


;; env-glob-symbol-macro-p returns a spec iff the symbol has a symbol-macro,
;; whose value can be found with env-glob-symbol-macro-val, or nil if
;; no symbol-macro is defined on the symbol.  Implementations should
;; strive to store the value in locative-fashion, so that a new locative
;; need not be consed for every access.

(defmacro env-glob-symbol-macro-p (symbol env)
  (declare (ignorable env))
  #+allegro `(ce-get ,symbol '.symbol-macro. ,env)
  #+cmu `(info variable macro-expansion ,symbol)
  #+clisp `(symbol-macro-p ,symbol)
  #+lispworks `(get ,symbol 'pc-symbol-macro-definition)
  #+sbcl `(sb-int::info :variable :macro-expansion ,symbol)
  #-(or allegro cmu clisp lispworks sbcl)
  (error "define env-glob-symbol-macro-p here." symbol))

(defmacro env-glob-symbol-macro-val (spec)
  #+(or allegro clisp) `,spec
  #+cmu `(list (cddr ,spec))
  #+(or lispworks sbcl) `(list ,spec)
  #-(or allegro cmu clisp lispworks sbcl)
  (error "define env-glob-symbol-macro-val here." spec))

(defmacro env-set-glob-symbol-macro (symbol value env)
  (declare (ignorable env))
  #+(or allegro clisp) `(ce-putprop ,symbol ,value '.symbol-macro. env)
  #+(or cmu sbcl) `(%define-symbol-macro ,symbol (car ,value))
  #+lispworks `(define-symbol-macro-aux ,symbol (car ,value))
  #-(or allegro cmu clisp lispworks sbcl)
  (error "define env-set-glob-symbol-macro here." symbol value))

(defmacro env-glob-compiler-macro-function (fspec env)
  (declare (ignorable env))
  #+allegro `(ce-get ,fspec '.compiler-macro. ,env)
  #+(or cmu clisp lispworks sbcl) `(compiler-macro-function ,fspec)
  #-(or allegro cmu clisp lispworks sbcl)
  (error "define env-glob-compiler-macro-function here." fspec))

(defmacro env-mark-function-as-macro (func)
  #+allegro `(excl::mark-function-as-macro ,func)
  #-allegro `,func)

(defmacro env-set-glob-compiler-macro (fspec definition env)
  (declare (ignorable env))
  #+allegro `(ce-putprop ,fspec ,definition '.compiler-macro. ,env)
  #+(or cmu clisp lispworks sbcl) `(setf (compiler-macro-function ,fspec) ,definition)
  #-(or allegro cmu clisp lispworks sbcl)
  (error "define env-set-glob-compiler-macro here." fspec definition))

;; fspec-equal-p compares two non-symbolic function-names.  Some CLs
;; have more than just (setf name) forms, and some (Allegro CL, in
;; particular) have interned and external representations for the names.

(defmacro fspec-equal-p (name1 name2)
  #+allegro `(excl::fspec-equal-p ,name1 ,name2)
  #-allegro `(equal ,name1 ,name2))

;; Env-function-name-p allows for more than just <symbol> and (setf <symbol>),
;; if the implementation supports it:

(defmacro env-function-name-p (name)
  #+allegro `(excl:function-name-p ,name)
  #-allegro (let ((n (gensym)))
	      `(let ((,n ,name))
		 (or (symbolp ,n)
		     (and (consp ,n)
			  (eq 'setf (car ,n))
			  (null (cddr ,n)))))))

;; Env-maphash-safe is like maphash, but knows (and does not need to test) that
;; the table argument really is a hash table.

(defmacro env-maphash-safe (fcn table)
  #+allegro `(excl::maphash-safe ,fcn ,table)
  #-allegro `(maphash ,fcn ,table))

;; env-named-function takes advantage of Allegro's named-function form,
;; which is just like a function form except it provides a name for
;; the function being created.

(defmacro env-named-function (name body)
  (declare (ignorable name))
  #+allegro `(named-function ,name ,body)
  #-allegro `(function ,body))

;; If possible, return a type in a normalized form.
(defmacro env-normalize-type (type &rest args)
  (declare (ignorable args))
  #+allegro `(excl:normalize-type ,type ,@args)
  #-allegro `,type)

;; Env-structurep tests to see that the arg is a structure (defined by defstruct)

(defmacro env-structurep (x)
  #+allegro `(excl::structurep ,x)
  #+cmu `(pcl::structurep ,x)
  #+clisp `(clos::structure-object-p ,x)
  #+lispworks `(lw:structurep ,x)
  #+sbcl `(augmentable-environment-p ,x)
  #-(or allegro cmu clisp lispworks sbcl)
  (error "define env-structurep here." x))

;; env-type-error creates a type-error condition:

(defmacro env-type-error (datum expected-type)
  #+allegro `(excl::.type-error ,datum ,expected-type)
  #-allegro `(error 'type-error :datum ,datum :expected-type ,expected-type
		    :format-control "~s is not of the expected type ~s."
		    :format-arguments (list ,datum ,expected-type)))

;; ===========  end of compatibility macros ==========


;; The compilation-unit environment is the environment which shadows the
;; lisp's environment while a compilation-unit is in progress.  Compile-file,
;; because it is also logically wrapped in a with-compilation-unit form,
;; should bind it to the result of make-compilation-unit-environment (below),
;; but only if it is not wrapped in another with-compilation-unit form,
;; and any compile-time-eval activity should bind it to nil while that
;; evaluation is being done.

(defvar *compilation-unit-environment* nil)

;;;; [deprecated: see note afterward]
;; The compile-file environment is the environment which shadows the
;; lisp's environment while a compile-file is in progress.  Compile-file
;; should bind it to the result of make-compile-file-environment (below)
;; and any compile-time-eval activity should bind it to nil while that
;; evaluation is being done.
;; [Note: Since a compile-file is really a specific case of a compilation
;; unit, this variable is now deprecated and the *compilation-unit-environment*
;; variable is used instead.  This variable now becomes a symbol-macro
;; which makes it a synonym for the new variable.]

(define-symbol-macro *compile-file-environment* *compilation-unit-environment*)


;; The interpreter environment (for lisps with an interpreter) is set up
;; whenever an evaluation occurs.  It is usually an :interpreter environment,
;; but it may be a :compiler environment which has been passed in
;; from a compilation-unit-environment while compile-time-eval is in effect.

(defvar *interpreter-environment* nil)


;; The evaluation environment is the dynamic portion of the environment
;; which came from the compile-file environment, in order to perform
;; compile-time evaluation.  This name is not very descriptive, but I
;; want to follow Ansi as much as possible.  I would have called it a
;; "shadow" environment...

(defvar *evaluation-environment* nil)


(defvar *base-gen* 0)

(defstruct (augmentable-environment-base
	     (:conc-name augmentable-environment-)
	     (:print-function .augmentable-environment-base-printer)
	     (:constructor make-augmentable-environment-base ()))
  (id (incf *base-gen*))
  (symbol-props nil)
  (non-symbol-props nil)
  (variable-hashtable
   (make-hash-table :test 'eq :size 19 :rehash-size 3.0))
  (function-hashtable
   (make-hash-table :test 'equal :size 19))
  (block-hashtable
   (make-hash-table :test 'eq :size 5))
  (tag-hashtable
   (make-hash-table :test 'eql :size 5))
  (declaration-hashtable
   (make-hash-table :test 'eq :size 5))
  (expression-hashtable nil))

(defun .augmentable-environment-base-printer (s stream d)
  (declare (ignore d))
  (print-unreadable-object (s stream :identity t)
    (format stream "~@<Augmentable Environment Base ~d (var: ~d fcn: ~d decl: ~d)~:@>"
	    (augmentable-environment-id s)
	    (hash-table-count (augmentable-environment-variable-hashtable s))
	    (hash-table-count (augmentable-environment-function-hashtable s))
	    (hash-table-count (augmentable-environment-declaration-hashtable s)))))

(defstruct (augmentable-environment
	     (:print-function .augmentable-environment-printer)
	     (:constructor make-augmentable-environment)
	     (:constructor internal-make-augmentable-environment-boa
			   (&optional (kindx :interpreter)
				      (link nil)
				      (base (make-augmentable-environment-base))
				      (index nil)
				      (index-generator 0))))
  (base (make-augmentable-environment-base))
  (index nil)
  (index-generator 0)
  (kindx :interpreter)			; One of :compiler, :compilation,
					; :interpreter, :evaluation
					; or :macros-only
  (link nil) ;; A more-global environment to search, or nil
  )

(defstruct (augmentable-compiler-environment
	     (:print-function .augmentable-environment-printer)
	     (:constructor nil))
  (base (make-augmentable-environment-base))
  (index nil)
  (index-generator 0)
  (kindx :interpreter)			; One of :compiler, :compilation,
					; :interpreter, :evaluation
					; or :macros-only
  (link nil) ;; A more-global environment to search, or nil
  )

(defstruct (augmentable-compilation-environment
	     (:print-function .augmentable-environment-printer)
	     (:constructor nil))
  (base (make-augmentable-environment-base))
  (index nil)
  (index-generator 0)
  (kindx :interpreter)			; One of :compiler, :compilation,
					; :interpreter, :evaluation
					; or :macros-only
  (link nil) ;; A more-global environment to search, or nil
  )

(defstruct (augmentable-evaluation-environment
	     (:print-function .augmentable-environment-printer)
	     (:constructor nil))
  (base (make-augmentable-environment-base))
  (index nil)
  (index-generator 0)
  (kindx :interpreter)			; One of :compiler, :compilation,
					; :interpreter, :evaluation
					; or :macros-only
  (link nil) ;; A more-global environment to search, or nil
  )

(defun make-augmentable-environment-boa (&optional (kind :interpreter)
					   (link nil)
					   (base (make-augmentable-environment-base))
					   (index nil)
					   (index-generator 0))
  (internal-make-augmentable-environment-boa kind link base index index-generator))

(defun .inv-augmentable-environment-kind (env value)
  (setf (augmentable-environment-kindx env) value))

(defsetf augmentable-environment-kind .inv-augmentable-environment-kind)

(defun augmentable-environment-kind (env)
  (augmentable-environment-kindx env))

(defun .augmentable-environment-printer (s stream d)
  (declare (ignore d))
  (print-unreadable-object (s stream :identity nil)
    (format stream "~@<Augmentable~@[ ~a~] environment~{ ~s~}~:@>"
	    (augmentable-environment-kind s)
	    (augmentable-environment-index s))))




;; Interface to the implementations: These functions will be
;; portable as long as get-property and put-property are correctly
;; defined.

(defun ce-get (obj ind &optional env)
  ;; like get except we look at the environment first
  (unless env
    (when (setq env *compilation-unit-environment*)
      (let* ((ht (if (symbolp obj)
		     (augmentable-environment-symbol-props
		      (augmentable-environment-base env))
		     (augmentable-environment-non-symbol-props
		      (augmentable-environment-base env))))
	     (rec (when ht (gethash obj ht)))
	     (ent (assoc ind rec :test #'eq)))
	(when ent
	  (return-from ce-get (cdr ent)))))
    (setq env *evaluation-environment*))
  (when env
    (let* ((ht (if (symbolp obj)
		   (augmentable-environment-symbol-props
		    (augmentable-environment-base env))
		   (augmentable-environment-non-symbol-props
		    (augmentable-environment-base env))))
	   (rec (when ht (gethash obj ht)))
	   (ent (assoc ind rec :test #'eq)))
      (when ent
	(return-from ce-get (cdr ent)))))
  (get-property obj ind))

(defsetf ce-get (obj ind &optional env) (val)
  `(ce-putprop ,obj ,val ,ind ,env))

(defun ce-putprop (obj value ind &optional env)
  ;; store on environment list if there is one
  (declare (optimize (speed 3)))
  (unless env
    (setq env
	  (or *compilation-unit-environment*
	      (and (let ((tmp *evaluation-environment*))
		     (when (and tmp
				(eq (augmentable-environment-kind tmp)
				    :evaluation))
		       tmp))))))
  (when env
    (let* ((ht (if (symbolp obj)
		   (augmentable-environment-symbol-props
		    (augmentable-environment-base env))
		   (augmentable-environment-non-symbol-props
		    (augmentable-environment-base env))))
	   rec ent)
      (when ht
	(setq rec (gethash obj ht))
	(setq ent (assoc ind rec :test #'eq))
	(if ent
	    (setf (cdr ent) value)
	    (push (cons ind value) (gethash obj ht)))
	(return-from ce-putprop value))))
  (put-property obj value ind)
  value)

;; Like ce-get, but does not look globally
(defun ce-get-from-env (obj ind env)
  (let* ((ht (if (symbolp obj)
		 (augmentable-environment-symbol-props
		  (augmentable-environment-base env))
		 (augmentable-environment-non-symbol-props
		  (augmentable-environment-base env))))
	 (rec (when ht (gethash obj ht)))
	 (ent (assoc ind rec :test #'eq)))
    (cdr ent)))


;; The guts of augment-environment:

(defvar *variable-declare-props* nil)

(defvar *function-declare-props* nil)

(defvar *declare-declare-props* nil)

(defun simple-augment-env-globally (env declare &optional name locative vtype ftype etype)
  (when (and vtype ftype)
    (error "Illegal global-env augmentation; only single-name bindings ~
with locatives allowed."))
  (when etype
    (error "Expression augmentation in global environment doesn't make sense."))
  (when vtype
    (case vtype
      (:constant
       (when env
	 (ce-putprop name locative '.constant-value. env)
	 (ce-putprop name t '.constant. env)
	 (when (and (numberp locative)
		    (not (assoc 'type declare)))
	   (push `(type ,(numeric-constant-type locative) ,name) declare))))
      (:symbol-macro
       (env-set-glob-symbol-macro name locative env))
      (t (error "Cannot handle ~s augmentation of the global environment for ~s." vtype name))))
  (when ftype
    (case ftype
      (:function
       (setf (fdefinition name) (car locative)))
      (:macro
       (setf (car locative) (env-mark-function-as-macro (car locative)))
       (if env
	   (ce-putprop name (car locative) '.compile-file-macro. env)
	   (setf (macro-function name) (car locative))))
      (:compiler-macro
       (env-set-glob-compiler-macro name (env-mark-function-as-macro (car locative)) env))
      (t (error "Cannot handle ~s augmentation of the global environment for ~s." ftype name))))
  (loop for d in declare
     do (multiple-value-bind (kind pdecls)
	    (parse-declaration d nil)
	  (ecase kind
	    (:variable
	     (loop for pd in pdecls
		do (let ((ent (assoc (cadr pd) *variable-declare-props* :test #'eq)))
		     (when ent
		       (ce-putprop (car pd) (caddr pd) (cdr ent) env)))))
	    (:function
	     (loop for pd in pdecls
		do (let ((ent (assoc (cadr pd) *function-declare-props* :test #'eq)))
		     (when ent
		       (ce-putprop (car pd) (caddr pd) (cdr ent) env)))))
	    (:both
	     (loop for pd in pdecls
		do (if (and (consp (car pd)) (eq 'function (caar pd)))
		       ;; A function declaration
		       (let ((ent (assoc (cadr pd) *function-declare-props* :test #'eq)))
			 (when ent
			   (ce-putprop (cadar pd) (caddr pd) (cdr ent) env)))
		       ;; A variable declaration
		       (let ((ent (assoc (cadr pd) *variable-declare-props* :test #'eq)))
			 (when ent
			   (ce-putprop (car pd) (caddr pd) (cdr ent) env))))))
	    (:declare
	     (let ((ent (assoc (car pdecls) *declare-declare-props* :test #'eq)))
	       (when ent
		 (ce-putprop (car pdecls) (cdr pdecls) (cdr ent) env)))))))
  nil)

(defun simple-augment-environment (environment declare reuse name locative vtype ftype &optional etype)
  ;; This version of the implementation of augment-environment is
  ;; much simpler than augment-environment-1, below, but it does not
  ;; handle multiple definitions, nor does it handle block and tag names.
  ;; 
  ;; Note that this function is never called in this module.  It is up
  ;; to a compiler macro (not given in this package) to transform a call
  ;; to augment-environment to a call to this function.
  (macrolet ((do-glob (env)
	       `(simple-augment-env-globally ,env declare name locative vtype ftype etype)))
    (unless environment
      (return-from simple-augment-environment
	(do-glob nil)))
    (let ((new-index (if reuse
			 (augmentable-environment-index environment)
			 (cons (incf (augmentable-environment-index-generator environment))
			       (augmentable-environment-index environment)))))
      (unless new-index
	(return-from simple-augment-environment
	  (do-glob environment)))
      (let* ((base (augmentable-environment-base environment))
	     (new (if reuse
		      environment
		      (make-augmentable-environment-boa
		       (augmentable-environment-kind environment)
		       environment base new-index)))
	     (vht (augmentable-environment-variable-hashtable base))
	     (fht (augmentable-environment-function-hashtable base))
	     (eht (augmentable-environment-expression-hashtable base)))
	(macrolet ((pushit (key type tab loc)
		     `(if (listp ,key)
			  (loop for v in ,key
			     do (if (consp v)
				    (let ((name (car v))
					  (value (cdr v)))
				      (unless value
					(setq value ,loc))
				      (push (list new-index (list* ,type value t))
					    (gethash name ,tab)))
				    (push (list new-index (list* ,type ,loc t))
					  (gethash v ,tab))))
			  (push (list new-index (list* ,type ,loc t))
				(gethash ,key ,tab)))))
	  (when vtype
	    (pushit name vtype vht locative))
	  (when ftype
	    (pushit name ftype fht locative))
	  (when etype
	    (unless eht
	      (setq eht (make-hash-table :test 'eq :size 50))
	      (setf (augmentable-environment-expression-hashtable base) eht))
	    (let* ((ents (gethash name eht))
		   (ent (assoc new-index ents))
		   (new-elem (list* etype locative t)))
	      (if ent
		  (push new-elem (cdr ent))
		  (push (list new-index new-elem) (gethash name eht)))))
	  (augment-declarations base new new-index vht fht declare))
	new))))

(defun augment-environment-1 (environment reuse locative
			      &optional variable symbol-macro
				function macro declare
				special-operator block tag
				constant flavor-iv compiler-macro expression)
  ;; This version of the implementation of augment-environment follows more
  ;; closely the intent of the CLtL2 definition, and it has some functionality
  ;; above and beyond the simple version above, but it is needed less than one
  ;; might expect, and it just has too many arguments!  A standardization effort
  ;; should reconsider the interface to augment-environment...
  (unless environment
    (return-from augment-environment-1
      (augment-env-globally nil locative variable symbol-macro
			    function macro declare special-operator
			    block tag constant flavor-iv compiler-macro expression)))
  (let ((new-index (if reuse
		       (augmentable-environment-index environment)
		       (cons (incf (augmentable-environment-index-generator environment))
			     (augmentable-environment-index environment)))))
    (unless new-index
      (return-from augment-environment-1
	(augment-env-globally environment locative variable symbol-macro
			      function macro declare special-operator
			      block tag constant flavor-iv compiler-macro expression)))
    (let* ((base (augmentable-environment-base environment))
	   (new (if reuse
		    environment
		    (make-augmentable-environment-boa
		     (augmentable-environment-kind environment)
		     environment base new-index)))
	   (vht (augmentable-environment-variable-hashtable base))
	   (fht (augmentable-environment-function-hashtable base))
	   (bht (augmentable-environment-block-hashtable base))
	   (tht (augmentable-environment-tag-hashtable base))
	   (eht (augmentable-environment-expression-hashtable base)))
      (macrolet ((pushit (key type tab loc &optional ident)
		   `(if (listp ,key)
			(loop for v in ,key
			   do (if (consp v)
				  (let ((name (car v))
					(value ,(if ident 'v '(cdr v))))
				    (unless value
				      (setq value ,loc))
				    (push (list new-index (list* ,type value t))
					  (gethash name ,tab)))
				  (push (list new-index (list* ,type ,loc t))
					(gethash v ,tab))))
			(push (list new-index (list* ,type ,loc t))
			      (gethash ,key ,tab)))))
	(pushit variable :lexical ;; start with this - may turn into special
		vht locative)
	(pushit symbol-macro :symbol-macro vht locative)
	(pushit constant :constant vht locative)
	(pushit flavor-iv :flavor-iv vht locative)
	(pushit function :function fht locative)
	(pushit macro :macro fht locative)
	(pushit special-operator :special-operator fht locative)
	(pushit block :block bht locative t) ;; We use the cons that holds the name as the value pointer
	(pushit tag :tag tht locative)
	(when expression
	  (unless eht
	    (setq eht (make-hash-table :test 'eq :size 50))
	    (setf (augmentable-environment-expression-hashtable base) eht))
	  (let* ((ents (gethash expression eht))
		 (ent (assoc new-index ents))
		 (new-elem (list* :expression locative t)))
	    (if ent
		(push new-elem (cdr ent))
		(push (list new-index new-elem) (gethash expression eht)))))
	(augment-declarations base new new-index vht fht declare))
      new)))

(defun augment-declarations (base new new-index vht fht declare)
  (let (bound-declarations)
    (when (and declare (consp declare) (eq (car declare) :bound))
      (setq bound-declarations t)
      (pop declare))
    (loop with ht = (augmentable-environment-declaration-hashtable base)
       for d in declare
       do (multiple-value-bind (kind pdecls)
	      (parse-declaration d new)
	    (case kind
	      (:declare
	       (push (list new-index (cdr pdecls)) (gethash (car pdecls) ht)))
	      (:variable
	       (loop for pd in pdecls
		  do (let ((ent (assoc new-index (gethash (car pd) vht) :test #'eq)))
		       (if (symbolp (car pd))
			   (progn
			     (unless (or ent bound-declarations)
			       ;; Free declaration
			       (multiple-value-bind (nkind valcell decl lex)
				   (variable-information
				    (car pd) (augmentable-environment-link new) t)
				 (if nkind
				     (setq ent (list* new-index (list* nkind valcell lex) decl))
				     (setq ent (list new-index (list* :free (list nil) lex)))))
			       (push ent (gethash (car pd) vht)))
			     (when ent
			       (let ((rec (cdr ent)))
				 (cond ((eq 'special (cadr pd))
					(setf (caar rec) :special))
				       ((eq 'lexical (cadr pd))
					(setf (caar rec) :lexical))
				       (t
					(push (cdr pd) (cdr rec)))))))
			   (warn "Ignoring bad variable name ~s in declaration ~s."
				 (car pd) d)))))
	      (:function
	       (loop for pd in pdecls
		  do (let ((ent (assoc new-index (gethash (car pd) fht) :test #'eq)))
		       (unless (or ent bound-declarations)
			 ;; Free declaration
			 (multiple-value-bind (nkind def decl lex)
			     (function-information
			      (car pd) (augmentable-environment-link new) t)
			   (if (and nkind
				    (not (and (eq :compiler-macro nkind)
					      (eq 'inline (cadr pd))
					      ;; [bug18187]: Treat both inline and notinline as free
					      #+ignore
					      (eq 'notinline (caddr pd)))))
			       (setq ent (list* new-index (list* nkind def lex) decl))
			       (setq ent (list new-index (list* :free (list nil) lex)))))
			 (push ent (gethash (car pd) fht)))
		       (when ent
			 (push (cdr pd) (cddr ent))))))
	      (:both
	       (loop for pd in pdecls
		  do (if (and (consp (car pd)) (eq 'function (caar pd)))
			 ;; A function declaration
			 (let ((ent (assoc new-index (gethash (cadar pd) fht) :test #'eq)))
			   (unless (or ent bound-declarations)
			     ;; Free declaration
			     (multiple-value-bind (nkind def decl lex)
				 (function-information
				  (cadar pd) (augmentable-environment-link new) t)
			       (if (and nkind
					(not (and (eq :compiler-macro nkind)
						  (eq 'inline (cadr pd))
						  ;; [bug18187]: Treat both inline and notinline as free
						  #+ignore
						  (eq 'notinline (caddr pd)))))
				   (setq ent (list* new-index (list* nkind def lex) decl))
				   (setq ent (list new-index (list* :free (list nil) lex)))))
			     (push ent (gethash (cadar pd) fht)))
			   (when ent				 
			     (push (cdr pd) (cddr ent))))
			 ;; A variable declaration
			 (if (symbolp (car pd))
			     (progn
			       (let ((ent (assoc new-index (gethash (car pd) vht) :test #'eq)))
				 (unless (or ent bound-declarations)
				   ;; Free declaration
				   (multiple-value-bind (nkind valcell decl lex)
				       (variable-information
					(car pd) (augmentable-environment-link new) t)
				     (if nkind
					 (setq ent (list* new-index (list* nkind valcell lex) decl))
					 (setq ent (list new-index (list* :free (list nil) lex)))))
				   (push ent (gethash (car pd) vht)))
				 (when ent
				   (let ((rec (cdr ent)))
				     (cond ((eq 'special (cadr pd))
					    (setf (caar rec) :special))
					   ((eq 'lexical (cadr pd))
					    (setf (caar rec) :lexical))
					   (t
					    (push (cdr pd) (cdr rec))))))))
			     (warn "Ignoring bad variable name ~s in declaration ~s."
				   (car pd) d))))))))))

(defun augment-env-globally (env locative variable symbol-macro function macro
			     declare special-operator block tag constant flavor-iv
			     compiler-macro &optional expression)
  (when (or block tag)
    (error "Illegal global-env augmentation; cannot augment block or tag types."))
  (let (error name vtype ftype etype)
    ;; This messing around is due to the unfortunate way the names are
    ;; given bindings.  I would choose to make them always conform to
    ;; simple-augment-environment, but multiple bindings at a time do
    ;; have their place.
    (when variable
      (setq name variable vtype :variable))
    (when (and (not error) symbol-macro)
      (when name (setq error t))
      (setq name symbol-macro vtype :symbol-macro))
    (when (and (not error) constant)
      (when name (setq error t))
      (setq name constant vtype :constant))
    (when (and (not error) flavor-iv)
      (when name (setq error t))
      (setq name flavor-iv vtype :flavor-iv))
    (when (and (not error) function)
      (when name (setq error t))
      (setq name function ftype :function))
    (when (and (not error) macro)
      (when name (setq error t))
      (setq name macro ftype :macro))
    (when (and (not error) special-operator)
      (when name (setq error t))
      (setq name special-operator ftype :special-operator))
    (when (and (not error) compiler-macro)
      (when name (setq error t))
      (setq name compiler-macro ftype :compiler-macro))
    (when (and (not error) expression)
      (when name (setq error t))
      (setq name expression etype :expression))
    (when (and name (null locative))
      (setq error t))
    (when error
      (error "Illegal global-env augmentation; only single-name bindings ~
with locatives allowed."))
    (simple-augment-env-globally env declare name locative vtype ftype etype)))


(defun augment-environment (environment
			    &key variable symbol-macro constant function macro declare
			      special-operator block tag flavor-iv reuse locative compiler-macro
			      expression)
  (augment-environment-1 environment reuse locative
			 variable symbol-macro function macro declare
			 special-operator block tag constant flavor-iv compiler-macro expression))

(defun augment-environment-as (environment kind)
  (let ((env (augment-environment environment)))
    (setf (augmentable-environment-kind env) kind)
    env))

(defun copy-environment (environment)
  ;; Creates a new environment, along with an environment base, which only
  ;; contains entries that are contained in the specified environment.
  ;; If the kind is :macros-only, then only the macro definitions are copied
  ;; (this is useful when making a lexical closure for macrolet; see the 
  ;; last paragraph in the dictionary page in the spec)
  ;;
  ;; If the copied environment ends up having no lexical entries in it,
  ;; nil is returned.

  (let* ((kind (augmentable-environment-kind environment))
	 (index (augmentable-environment-index environment))
	 (new-env (make-augmentable-environment
		   :index index
		   :index-generator (augmentable-environment-index-generator environment)
		   :kindx kind))
	 (old-base (augmentable-environment-base environment))
	 (new-base (augmentable-environment-base new-env))
	 (macros-only nil)
	 (empty t))
    (when (eq :macros-only kind)
      (setq macros-only t)
      (setf (augmentable-environment-kind new-env) :interpreter))
    (flet ((copy-table (old new &optional macros-only)
	     (maphash
	      #'(lambda (k v)
		  (do ((ent v (cdr ent))
		       (res nil))
		      ((null ent)
		       (when res
			 (setf (gethash k new)
			       (nreverse res))))
		    (when (enclosed-in-environment-p (caar ent) index)
		      (when (or (not macros-only)
				(member (caadar ent) ;; [bug16265]
					'(:macro :symbol-macro) :test #'eq))
			(when empty (setq empty nil))
			(push (car ent) res)))))
	      old)))
      (copy-table (augmentable-environment-variable-hashtable old-base)
		  (augmentable-environment-variable-hashtable new-base)
		  macros-only)
      (copy-table (augmentable-environment-function-hashtable old-base)
		  (augmentable-environment-function-hashtable new-base)
		  macros-only)
      (unless macros-only
	(copy-table (augmentable-environment-block-hashtable old-base)
		    (augmentable-environment-block-hashtable new-base))
	(copy-table (augmentable-environment-tag-hashtable old-base)
		    (augmentable-environment-tag-hashtable new-base))
	(copy-table (augmentable-environment-declaration-hashtable old-base)
		    (augmentable-environment-declaration-hashtable new-base))))
    (if empty
	nil
	new-env)))

;; [bug16054]: Add a function to check for empty lexical environment
(defun empty-lexical-environment-p (environment)
  ;; Checks the environment argument to see whether there are any entries
  ;; that are contained in the specified environment.
  ;; If the environment ends up having no lexical entries in it,
  ;; t is returned, otherwise nil is returned.

  (or (null environment)
      (let* ((index (augmentable-environment-index environment))
	     (base (augmentable-environment-base environment)))
	(flet ((test-table (ht)
		 (maphash
		  #'(lambda (k v)
		      (declare (ignore k))
		      (do ((ent v (cdr ent)))
			  ((null ent))
			(when (enclosed-in-environment-p (caar ent) index)
			  (return-from empty-lexical-environment-p nil))))
		  ht)))
	  (test-table (augmentable-environment-variable-hashtable base))
	  (test-table (augmentable-environment-function-hashtable base))
	  (test-table (augmentable-environment-block-hashtable base))
	  (test-table (augmentable-environment-tag-hashtable base))
	  (test-table (augmentable-environment-declaration-hashtable base)))
	t)))

(defun make-compilation-unit-environment (&optional compile)
  ;; Make an environment suitable for compile or compile-file
  ;; If compile is true, then the environment only has a lexical
  ;; component (suitable for compile)
  (let* ((env (make-augmentable-environment-boa :compiler))
	 (base (augmentable-environment-base env)))
    (unless compile ;; [rfe6136]
      (setf (augmentable-environment-symbol-props base)
	    (make-hash-table :test 'eq :size 100 :rehash-size 3.0))
      (setf (augmentable-environment-non-symbol-props base)
	    (make-hash-table :test 'equal :size 19)))
    env))

(defun make-compile-file-environment ()
  ;; Deprecated; make an environment suitable for compile-file
  (declare (optimize speed (safety 0) (debug 0)))
  (make-compilation-unit-environment))

(defun ensure-portable-walking-environment (environment)
  (when (and environment
	     (eq (augmentable-environment-kind environment) :compiler))
    ;; If it is a :compiler environment, augment and convert it to a
    ;; :compilation environment
    (setq environment (augment-environment-as environment :compilation)))
  (or environment
      (make-augmentable-environment-boa :interpreter)))

(defun pop-environment (environment)
  ;; Removes all entries from the environment base that are newer than
  ;; the specified environment.
  (declare (optimize speed))
  (let ((base (augmentable-environment-base environment)))
    (flet ((pop-table (table)
	     (env-maphash-safe
	      #'(lambda (k v)
		  (block xxx
		    (let ((ll v)
			  (prev nil)
			  (index (augmentable-environment-index environment))
			  (changed nil))
		      (loop (when (null v)
			      (remhash k table)
			      (return-from xxx))
			 (if (newer-than-environment-p (caar v) index)
			     (setq v (cdr v)
				   changed t)
			     (return nil)))
		      (setq prev v
			    ll (cdr v))
		      (loop (when (null ll)
			      (when changed
				(setf (gethash k table) v))
			      (return))
			 (if (newer-than-environment-p (caar ll) index)
			     (setf (cdr prev) (setq ll (cdr ll))
				   changed t)
			     (setq prev ll
				   ll (cdr ll)))))))
	      table)))
      (pop-table (augmentable-environment-variable-hashtable base))
      (pop-table (augmentable-environment-function-hashtable base))
      (pop-table (augmentable-environment-block-hashtable base))
      (pop-table (augmentable-environment-tag-hashtable base))
      (pop-table (augmentable-environment-declaration-hashtable base)))
    environment))

(defun clear-lexical-environment (environment)
  ;; Removes all lexically-oriented entries from the environment base
  (declare (optimize speed))
  (let ((base (augmentable-environment-base environment)))
    (clrhash (augmentable-environment-variable-hashtable base))
    (clrhash (augmentable-environment-function-hashtable base))
    (clrhash (augmentable-environment-block-hashtable base))
    (clrhash (augmentable-environment-tag-hashtable base))
    (clrhash (augmentable-environment-declaration-hashtable base)))
  environment)


(defun parse-declaration (declaration env)
  (unless (listp declaration)
    (env-type-error declaration 'list))
  (let* ((decl-spec (car declaration))
	 (handler (and (symbolp decl-spec)
		       (get decl-spec 'declaration-handler))))
    (cond (handler
	   (funcall handler declaration env))
	  ((let ((not-a-type '#:not-a-type))
	     (not (eq not-a-type
		      (env-normalize-type decl-spec
					  :default not-a-type
					  :environment *compilation-unit-environment*))))
	   ;; The car is an abbreviated type declaration
	   (funcall (get 'type 'declaration-handler)
		    (cons 'type declaration)
		    env))
	  (t (warn "no declaration defined for ~s in ~s" (car declaration) declaration)
	     :declare ;; I.e. (values :declare nil)
	     ))))

(defun enclosed-in-environment-p (entry-key environment-key)
  (declare (optimize speed (safety 0) (debug 0)))
  ;; length calculations done without numbers
  (or (eq entry-key environment-key)
      (do ((ent entry-key (cdr ent))
	   (env environment-key (cdr env)))
	  ((null ent)
	   (do ((tail env (cdr tail))
		(head environment-key (cdr head)))
	       ((null tail)
		(equal entry-key head))))
	(when (null env)
	  (return nil)))))

(defun newer-than-environment-p (entry-key environment-key)
  (declare (optimize speed (safety 0) (debug 0)))
  (do ((ent (cdr entry-key) (cdr ent)))
      ((null ent))
    (when (eq ent environment-key)
      (return t))))

(macrolet
    ((get-environment-info (name environment table)
       `(let ((ekey (augmentable-environment-index ,environment)))
	  (do ((ent (gethash ,name ,table) (cdr ent)))
	      ((enclosed-in-environment-p (caar ent) ekey)
	       ent)))))
  (defun get-environment-variable-info (symbol environment)
    (declare (optimize speed (safety 0)))
    (get-environment-info
     symbol environment
     (augmentable-environment-variable-hashtable
      (augmentable-environment-base environment))))
  (defun get-environment-function-info (fspec environment)
    (declare (optimize speed (safety 0)))
    (get-environment-info
     fspec environment
     (augmentable-environment-function-hashtable
      (augmentable-environment-base environment))))
  (defun get-environment-declaration-info (symbol environment)
    (declare (optimize speed (safety 0)))
    (get-environment-info
     symbol environment
     (augmentable-environment-declaration-hashtable
      (augmentable-environment-base environment))))
  (defun get-environment-block-info (symbol environment)
    (declare (optimize speed (safety 0)))
    (get-environment-info
     symbol environment
     (augmentable-environment-block-hashtable
      (augmentable-environment-base environment))))
  (defun get-environment-tag-info (symbol environment)
    (declare (optimize speed (safety 0)))
    (get-environment-info
     symbol environment
     (augmentable-environment-tag-hashtable
      (augmentable-environment-base environment)))))

;; This one's separate because of the special handling of the key - if the
;; key is 't, then the current raw data (i.e. for all entries of this expression)
;; is returned, otherwise only the innermost entry is returned as a normal
;; *-information record.
(defun get-environment-expression-info (expression environment ekey)
  (declare (optimize speed (safety 0)))
  (let ((table
	 (augmentable-environment-expression-hashtable
	  (augmentable-environment-base environment))))
    (when table
      (if (eq ekey t)
	  (gethash expression table)
	  (do ((ent (gethash expression table) (cdr ent)))
	      ((enclosed-in-environment-p (caar ent) ekey)
	       ent))))))

;; Variables

(defun variable-information (symbol &optional environment all-declarations)
  (declare (optimize speed (safety 0)))
  (cond ((env-structurep environment)
	 (let ((res (get-environment-variable-info symbol environment)))
	   (if (setq res (car res))
	       (let* ((ent (cadr res))
		      (lex (when (cddr ent)
			     (if (eq (car res) (augmentable-environment-index environment))
				 t
				 :outer))))
		 (values (car ent) (cadr ent) (cddr res) lex))
	       ;; Look at global environment as well.
	       (let ((temp (env-glob-symbol-macro-p symbol environment))
		     (type :free)
		     decls)
		 (if all-declarations
		     (loop for x in *variable-declare-props*
			do (let ((res (ce-get symbol (cdr x) environment)))
			     (when res
			       (if (eq (car x) 'special)
				   (setq type :special)
				   (push (list (car x) res) decls)))))
		     (when (ce-get symbol '.globally-special. environment)
		       (setq type :special)))
		 (cond (temp
			(values :symbol-macro (env-glob-symbol-macro-val temp) decls))
		       ((ce-get symbol '.constant. environment)
			(let ((value (ce-get symbol '.constant-value. environment)))
			  (when (and all-declarations
				     (numberp value)
				     (not (assoc 'type decls)))
			    (push `(type ,(numeric-constant-type value)) decls))
			  (values :constant
				  value
				  decls)))
		       ((eq type :special)
			(values type nil decls))
		       (t
			(when (and all-declarations decls)
			  (values type (list nil) decls))))))))
	(environment
	 (warn "variable-information got bogus environment ~s" environment))
	(t
	 (let ((temp (env-glob-symbol-macro-p symbol environment))
	       (type :free)
	       decls)
	   (if all-declarations
	       (loop for x in *variable-declare-props*
		  do (let ((res (ce-get symbol (cdr x))))
		       (when res
			 (if (eq (car x) 'special)
			     (setq type :special)
			     (push (list (car x) res) decls)))))
	       ;; [bug15692]
	       (when (ce-get symbol '.globally-special. environment)
		 (setq type :special)))
	   (cond (temp
		  (values :symbol-macro (env-glob-symbol-macro-val temp) decls))
		 ((ce-get symbol '.constant.)
		  (let ((value (ce-get symbol '.constant-value.)))
		    (when (and all-declarations
			       (numberp value)
			       (not (assoc 'type decls)))
		      (push `(type ,(numeric-constant-type value)) decls))
		    (values :constant
			    (ce-get symbol '.constant-value.)
			    decls)))
		 ((eq type :special)
		  (values type nil decls))
		 (t
		  (when decls
		    (values type (list nil) decls))))))))

(defun numeric-constant-type (number)
  (let ((type (type-of number)))
    (cond ((or (eq type 'fixnum) (eq type 'bignum))
	   (list 'integer number number))
	  ((eq type 'complex)
	   (list 'complex number))
	  (t (list type number number)))))

(defun environment-variable-count (environment)
  (hash-table-count (augmentable-environment-variable-hashtable
		     (augmentable-environment-base environment))))

(defun nth-variable (index environment)
  ;; Gets the index'th variable name from the environment.
  ;; This is only good for debugging; the index really means nothing
  ;; in any other sense.
  (declare (fixnum index))
  (when (>= index 0)
    (maphash
     #'(lambda (k v)
	 (when (zerop index)
	   (return-from nth-variable
	     (values k (caadar v) (car (cdadar v)))))
	 (decf index))
     (augmentable-environment-variable-hashtable
      (augmentable-environment-base environment)))))

;; Functions

(defun function-information (fspec &optional environment
				     all-declarations
				     special-operators)
  (declare (optimize speed (safety 0)))
  (cond ((env-structurep environment)
	 (let* ((res (get-environment-function-info fspec environment))
		def decls)
	   (when (car res)
	     (let ((ent (cadar res)))
	       (unless (eq :free (car ent))
		 (return-from function-information
		   (values (car ent) (cadr ent) (cddar res) (cddr ent))))
	       ;; we may have to deal with lexically shadowed
	       ;; items under this free declaration.
	       (setq decls (cddar res))))
	   ;; Look at global environment as well.
	   (when all-declarations
	     (loop for x in *function-declare-props*
		do (let ((res (ce-get fspec (cdr x) environment)))
		     (when res
		       (push (list (car x) res) decls)))))
	   (cond ((and special-operators
		       (special-operator-p fspec))
		  (values :special-operator nil decls)) ;; ... nil
		 ((and (eq (augmentable-environment-kind environment)
			   :compiler)
		       (setq def
			     (env-glob-compiler-macro-function fspec environment)))
		  (values
		   :compiler-macro
		   (and all-declarations (list def))
		   decls
		   ;; ... nil nil
		   ))
		 ((and (member (augmentable-environment-kind environment)
			       '(:compiler :compilation))
		       (setq def (ce-get fspec '.compile-file-macro. environment)))
		  (values
		   :macro
		   (and all-declarations (list def))
		   decls
		   ;; ... nil nil
		   ))
		 ((setq def (and (symbolp fspec) (macro-function fspec)))
		  (values
		   :macro
		   (and all-declarations (list def))
		   decls
		   ;; ... nil nil
		   ))
		 ((setq def (fboundp fspec))
		  (let ((spop (special-operator-p fspec)))
		    (values
		     (if spop
			 :special-operator
			 :function)
		     (and all-declarations (not spop) (list def))
		     decls
		     ;; nil
		     )))
		 (all-declarations
		  (when decls
		    (values :free (list nil) decls))))))
	(environment
	 (warn "function-information got bogus environment ~s" environment))
	(t
	 (let (def decls)
	   (when all-declarations
	     (loop for x in *function-declare-props*
		do (let ((res (ce-get fspec (cdr x))))
		     (when res
		       (push (list (car x) res) decls)))))
	   (cond ((and special-operators
		       (special-operator-p fspec))
		  (values :special-operator nil decls)) ;; ... nil
		 ((setq def (and (symbolp fspec) (macro-function fspec)))
		  (values
		   :macro
		   (and all-declarations (list def))
		   decls
		   ;; ... nil nil
		   ))
		 ((setq def (fboundp fspec))
		  (let ((spop (special-operator-p fspec)))
		    (values
		     (if spop
			 :special-operator
			 :function)
		     (and all-declarations (not spop) (list def))
		     decls
		     ;; nil
		     )))
		 (all-declarations
		  (when decls
		    (values :free (list nil) decls))))))))

;; Declarations

(defun declaration-information (symbol &optional environment)
  (declare (optimize speed (safety 0)))
  (cond ((env-structurep environment)
	 (let ((res (get-environment-declaration-info symbol environment)))
	   (if (setq res (car res))
	       (cadr res)
	       (let ((prop (cdr (assoc symbol *declare-declare-props* :test #'eq))))
		 (and prop (ce-get symbol prop environment))))))
	(environment
	 (warn "Declaration-information got bogus environment ~s" environment))
	(t (let ((prop (cdr (assoc symbol *declare-declare-props* :test #'eq))))
	     (and prop (ce-get symbol prop))))))
;; Blocks

(defun block-information (symbol &optional environment)
  (declare (optimize speed (safety 0)))
  (when (env-structurep environment)
    (let ((res (get-environment-block-info symbol environment)))
      (when (setq res (car res))
	(values (caadr res) (cadadr res) (caddr res)))))
  ;; Need to merge in global environment information.
  )

;; Tags

(defun tag-information (symbol &optional environment)
  (declare (optimize speed (safety 0)))
  (when (env-structurep environment)
    (let ((res (get-environment-tag-info symbol environment)))
      (when (setq res (car res))
	(values (caadr res) (cadadr res) (caddr res))))))

(defun expression-information (expression &optional environment
					    (ekey (augmentable-environment-index environment)))
  (declare (optimize speed (safety 0)))
  (when (env-structurep environment)
    (let ((res (get-environment-expression-info expression environment ekey)))
      (if (eq ekey t)
	  ;; return all of the entries for this expression
	  res
	  ;; Return the information embedded in this contour.
	  (when (setq res (car res))
	    (values (caadr res) (cadadr res) (caddr res)))))))

(defun expression-entries (expression &optional environment)
  (let ((table 
	 (augmentable-environment-expression-hashtable
	  (augmentable-environment-base environment))))
    (when table
      (values (gethash expression table)))))

;; Mapping functions:

(defun map-over-environment-variables (fcn environment)
  ;; Calls fcn with the top definition only of each variable in env.
  ;; The order of definition of the variables is lost.
  ;;  This is only intended to work for interpreted environments, and
  ;; assumes that the environment-popping is being done properly by
  ;; the interpreter (thus leaving the current value of the variable
  ;; at the top of the hash value).
  (when environment
    (maphash
     #'(lambda (k v)
	 (let ((temp (cadar v)))
	   (funcall fcn k (car temp) (cadr temp))))
     (augmentable-environment-variable-hashtable
      (augmentable-environment-base environment)))))

(defun map-over-environment-functions (fcn environment)
  ;; Calls fcn with the top definition only of each function in env.
  ;; The order of definition of the variables is lost.
  ;;  This is only intended to work for interpreted environments, and
  ;; assumes that the environment-popping is being done properly by
  ;; the interpreter (thus leaving the current value of the function
  ;; at the top of the hash value).
  (when environment
    (maphash
     #'(lambda (k v)
	 (let ((temp (cadar v)))
	   (funcall fcn k (car temp) (cadr temp))))
     (augmentable-environment-function-hashtable
      (augmentable-environment-base environment)))))

(defun map-over-current-environment-variables (fcn environment)
  ;; Calls fcn with the top definition only of each variable in env.
  ;; The order of definition of the variables is lost.
  ;;  This is only intended to work for interpreted environments
  (when environment
    (let ((ekey (augmentable-environment-index environment)))
      (maphash
       #'(lambda (k v)
	   (let ((res (dolist (ent v)
			(when (enclosed-in-environment-p (car ent) ekey)
			  (return (cdr ent))))))
	     (when res
	       (funcall fcn k (caar res) (cadar res)))))
       (augmentable-environment-variable-hashtable
	(augmentable-environment-base environment))))))

(defun map-over-environment-items (function indicator environment)
  ;; map the function over all items with the given indicator
  ;; within the environment.
  ;; the function should take one argument:
  ;;    the value
  (declare (optimize speed))
  (when environment
    (let (props)
      (flet ((mapit (k v)
	       (declare (ignore k))
	       (let ((ent (assoc indicator v :test #'eq)))
		 (when ent
		   (funcall function (cdr ent))))))
	(when (setq props
		    (augmentable-environment-symbol-props
		     (augmentable-environment-base environment)))
	  (maphash #'mapit props))
	(when (setq props
		    (augmentable-environment-non-symbol-props
		     (augmentable-environment-base environment)))
	  (maphash #'mapit props))))))

;; define-declaration and support
;;

;; [rfe7999]
(defun map-over-environment-blocks (fcn environment)
  ;; Calls fcn with the top definition only of each block name in env.
  ;; The order of definition of the variables is lost.
  ;;  This is only intended to work for interpreted environments, and
  ;; assumes that the environment-popping is being done properly by
  ;; the interpreter (thus leaving the current value of the variable
  ;; at the top of the hash value).
  (when environment
    (maphash
     #'(lambda (k v)
	 (let ((temp (cadar v)))
	   (funcall fcn k (car temp) (cadr temp))))
     (augmentable-environment-block-hashtable
      (augmentable-environment-base environment)))))

(defun map-over-environment-tags (fcn environment)
  ;; Calls fcn with the top definition only of each tag name in env.
  ;; The order of definition of the variables is lost.
  ;;  This is only intended to work for interpreted environments, and
  ;; assumes that the environment-popping is being done properly by
  ;; the interpreter (thus leaving the current value of the variable
  ;; at the top of the hash value).
  (when environment
    (maphash
     #'(lambda (k v)
	 (let ((temp (cadar v)))
	   (funcall fcn k (car temp) (cadr temp))))
     (augmentable-environment-tag-hashtable
      (augmentable-environment-base environment)))))


(defun get-default-declaration-def (name kind)
  (declare (optimize (speed 2)))
  (env-named-function default-binding-declaration-handler
		      (lambda (declaration env)
			(declare (ignore env))
			(values
			 kind
			 (if (eq :declare kind)
			     (if (cdr declaration)
				 (cons (car declaration) (cadr declaration))
				 (cons (car declaration) t))
			     (let ((spec (list name (car declaration))))
			       (mapcar #'(lambda (x) (cons x spec))
				       (cdr declaration))))))))

(defun syntaxify-declaration-lambda-list (lambda-list)
  ;; Arglist is a simple lambda-list.  Turn it into enough syntax information
  ;; that it can be used for extracting and analyzing declaration specifiers.
  (let (res found-rest)
    (do ((ll lambda-list (cdr ll)))
	((null ll) (nreverse res))
      (cond ((eq '&rest (car ll))
	     (setq found-rest t))
	    ((member (car ll) '(&key &allow-other-keys &aux) :test #'eq)
	     (error "illegal lambda-list keyword ~s in ~s." (car ll) lambda-list))
	    (found-rest
	     (push :names res)
	     (setq found-rest nil))
	    ((not (eq (car ll) '&optional))
	     (push (car ll) res))))))


(defmacro define-declaration (namespec lambda-list prop kind &optional def)
  (let ((valid '(:variable :function :both :declare))
	(name (if (consp namespec) (car namespec) namespec))
	(lnames (if (consp namespec) (cdr namespec) (list namespec)))
	rdef syntax)
    (unless (listp lambda-list)
      (env-type-error lambda-list 'list))
    (when (or (consp prop) (consp kind))
      (error "Both prop: ~s and kind: ~s args must be symbols." prop kind))
    (setq syntax (syntaxify-declaration-lambda-list lambda-list))
    (setq rdef (if def
		   (if (symbolp def)
		       `',def
		       `(env-named-function ,(intern (concatenate 'simple-string
								  (symbol-name 'declaration-handler-for-)
								  (symbol-name name)))
					    ,def))
		   `(get-default-declaration-def ',name ',kind)))
    `(progn
       (eval-when (:compile-toplevel :execute)
	 (unless (member ,kind ',valid)
	   (error "Kind argument ~s not one of ~s." ,kind ',valid)))
       ,@(when prop
	       (case kind
		 (:variable
		  `((pushnew (cons ',name ',prop) *variable-declare-props*
			     :test #'equal)))
		 (:function
		  `((pushnew (cons ',name ',prop) *function-declare-props*
			     :test #'equal)))
		 (:both
		  `((pushnew (cons ',name ',prop) *variable-declare-props*
			     :test #'equal)
		    (pushnew (cons ',name ',prop) *function-declare-props*
			     :test #'equal)))
		 (:declare
		  `((pushnew (cons ',name ',prop) *declare-declare-props*
			     :test #'equal)))))
       ,@(loop for lname in lnames
	    collect `(pushnew ',lname (get 'declaration '.declaration.))
	    collect `(setf (get ',lname 'declaration-kind) ,kind)
	    collect `(setf (get ',lname 'declaration-handler) ,rdef)
	    collect `(setf (get ',lname 'declaration-syntax) ',syntax))
       ',name)))

(defun declaration-names-position (decl)
  ;; Returns the position of the first lexical name in the
  ;; declaration, or nil if there is none.
  (let ((syntax (get (car decl) 'declaration-syntax)))
    (position :names syntax)))

(defun extract-declaration (name decl-spec)
  ;; Handles var and fname names, and also (function fname)
  ;; This function assumes that the top-level of the lists it
  ;; is working on are not constants (i.e. are mutable)
  (let* ((syntax (get (car decl-spec) 'declaration-syntax))
	 (pos (position :names syntax :test #'eq)))
    (when pos
      (let ((rpos (1+ pos))
	    (tail (nthcdr pos decl-spec)))
	(do ((tt tail (cdr tt)))
	    ((null (cdr tt)))
	  (let ((var (cadr tt)))
	    (when (or (and (symbolp var)
			   (eq name var))
		      (and (consp var)
			   (eq (car  var) 'function)
			   (fspec-equal-p (cadr var) name)))
	      (let ((head (subseq decl-spec 0 rpos)))
		(setf (cdr tt) (cddr tt))
		(return-from extract-declaration
		  (values (unless (null (cdr tail))
			    decl-spec)
			  (nconc head (list var))))))))))
    decl-spec))

(defun find-in-declaration (name decl-spec)
  ;; Like extract-declaration, but does not modify decl-spec
  ;; Handles var and fname names, and also (function fname)
  (let* ((syntax (get (car decl-spec) 'declaration-syntax))
	 (pos (position :names syntax :test #'eq)))
    (when pos
      (let* ((rpos (1+ pos))
	     (tail (nthcdr rpos decl-spec)))
	(do ((tt tail (cdr tt)))
	    ((null tt))
	  (let ((var (car tt)))
	    (when (or (and (symbolp var)
			   (eq name var))
		      (and (consp var)
			   (eq (car  var) 'function)
			   (fspec-equal-p (cadr var) name)))
	      (return-from find-in-declaration t))))))))

(defun declaration-kind (decl)
  (ce-get (car decl) 'declaration-kind))

;; Possible implementation of constantp:
#+allegro
(defun constantp (form &optional environment)
  (typecase form
    (null t)
    (symbol
     (multiple-value-bind (kind valcell)
	 (variable-information form environment t)
       (case kind
	 (:constant t)
	 (:symbol-macro (constantp (car valcell) environment)))))
    (cons
     (cond ((and (eq (car form) 'quote)
		 (cdr form)
		 (null (cddr form)))
	    t)
	   ((and (env-function-name-p (car form))
		 (eq (function-information (car form) environment nil t)
		     :macro))
	    (constantp (macroexpand form environment #+allegro t) environment))))
    (t t)))

(defun constant-value (form &optional environment)
  (typecase form
    (null nil)
    (symbol
     (multiple-value-bind (kind valcell)
	 (variable-information form environment t)
       (case kind
	 (:constant valcell)
	 (:symbol-macro (constant-value (car valcell) environment)))))
    (cons
     (cond ((and (eq (car form) 'quote)
		 (cdr form)
		 (null (cddr form)))
	    (cadr form))
	   ((and (env-function-name-p (car form))
		 (eq (function-information (car form) environment nil t)
		     :macro))
	    (constant-value (macroexpand form environment #+allegro t)
			    environment))))
    (t form)))

;; Declarations moved to defdecl.cl

;; Test of variable-information:
#|
(defmacro with-vi-test ((var) &body body &environment e)
  (format t "variable-information ~s is ~s~%"
	  var (multiple-value-list (variable-information var e)))
  `(progn ,@body))

(defconstant seven 7)

(defvar *spec*)

(define-symbol-macro not-eleven (1- seven))


(let ((x (multiple-value-list (get-universal-time))))
  (declare (special frob))
  (symbol-macrolet ((pie 22/7))
    (defun load-time (&aux (*spec* 5))
      (declare (special *spec*))
      (with-vi-test (x)
	(with-vi-test (frob)
	  (with-vi-test (*spec*)
	    (with-vi-test (*print-base*)
	      (with-vi-test (seven)
		(with-vi-test (not-eleven)
		  (+ seven x))))))))
    (defun apple-pie (apple)
      (with-vi-test (apple)
	(with-vi-test (pie)
	  (let ((z (* apple pie)))
	    (declare (type number z))
	    (with-vi-test (pi)
	      (with-vi-test (unknown)
		(with-vi-test (z)
		  (print (values pi z)))))))))))
|#

;; Basic implementations for non Allegro CL versions:

#-allegro
(progn

  (defvar *property-hash-table*
    (make-hash-table :size 100 :test #'equal))

  ;; Like putprop and (setf get), except:
  ;;  1. objects can be lists (or any other objects) as well as
  ;;     symbols
  ;;  2. if the value is nil, then the property is removed, as if with
  ;;     remprop, instead of being set to a nil value.
  (defun put-property-impl (obj value ind)
    (if (symbolp obj)
	;; If property value is NIL, remprop instead of setf
	(unless (eq ind '.constant-value.)
	  ;; Don't set constant-value; defconstant should do this.
	  (if value
	      (setf (get obj ind) value)
	      (remprop obj ind)))
	;; Non-symbol property:
	(let* ((rec (gethash obj *property-hash-table*))
	       (ent (assoc ind rec :test #'eq)))
	  (cond ((and ent value)
		 (setf (cdr ent) value))
		(value
		 (push (cons ind value) (gethash obj *property-hash-table*)))
		(t ;; If value is nil, remove instead of setf
		 (let ((newent (delete ind rec :key #'car)))
		   (if newent
		       (setf (gethash obj *property-hash-table*) newent)
		       (remhash obj *property-hash-table*)))))))
    value)

  (defun get-property-impl (obj ind)
    (if (symbolp obj)
	(if (eq ind '.constant-value.)
	    (when (get-property-impl obj '.constant.)
	      (symbol-value obj))
	    (get obj ind))
	(let* ((rec (gethash obj *property-hash-table*))
	       (ent (assoc ind rec :test #'eq)))
	  (when ent
	    (cdr ent)))))

  )


#+cmu
(progn
  (eval-when (:compile-toplevel)
    (setq lisp::*enable-package-locked-errors* cl-user::.saved-p-lock.))
  (eval-when (:load-toplevel :execute)
    (setq lisp::*enable-package-locked-errors* cl-user::.saved-p-lock.)))

#+clisp
(progn
  (eval-when (:compile-toplevel)
    (setf (package-lock cl-user::.saved-p-lock.) t))
  (eval-when (:load-toplevel :execute)
    (setf (package-lock cl-user::.saved-p-lock.) t)))
