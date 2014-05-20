;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;-*- Mode:LISP; Package:(WALKER LISP 1000); Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************
;;; 
;;; A simple code walker, based IN PART on: (roll the credits)
;;;   Larry Masinter's Masterscope
;;;   Moon's Common Lisp code walker
;;;   Gary Drescher's code walker
;;;   Larry Masinter's simple code walker
;;;   .
;;;   .
;;;   boy, thats fair (I hope).
;;;
;;; For now at least, this code walker really only does what PCL needs it to
;;; do.  Maybe it will grow up someday.
;;;

;;;
;;; This code walker used to be completely portable.  Now it is just "Real
;;; easy to port".  This change had to happen because the hack that made it
;;; completely portable kept breaking in different releases of different
;;; Common Lisps, and in addition it never worked entirely anyways.  So,
;;; its now easy to port.  To port this walker, all you have to write is one
;;; simple macro and two simple functions.  These macros and functions are
;;; used by the walker to manipluate the macroexpansion environments of
;;; the Common Lisp it is running in.
;;;
;;; The code which implements the macroexpansion environment manipulation
;;; mechanisms is in the first part of the file, the real walker follows it.
;;; 

;;;
;;; The user entry points are walk-form and nested-walked-form.  In addition,
;;; it is legal for user code to call the variable information functions:
;;; variable-lexical-p, variable-special-p and variable-class.  Some users
;;; will need to call define-walker-template, they will have to figure that
;;; out for themselves.
;;; 
(defpackage "WALKER"
  (:export define-walker-template
	   walk-form
	   walk-form-expand-macros-p
	   #-ecl nested-walk-form
	   variable-lexical-p
	   variable-special-p
	   *variable-declarations*
	   variable-declaration
	   macroexpand-all
	   )
  (:import-from "SI" "GET-SYSPROP" "PUT-SYSPROP"))

(in-package "WALKER")
(declaim (notinline note-lexical-binding walk-bindings-1 walk-let/let*
		    walk-form-internal))
(push :new *features*)


;;;
;;; On the following pages are implementations of the implementation specific
;;; environment hacking functions.
;;;
;;; This code just hacks 'macroexpansion environments'.  That is, it is only
;;; concerned with the function binding of symbols in the environment.  The
;;; walker needs to be able to tell if the symbol names a lexical macro or
;;; function, and it needs to be able to build environments which contain
;;; lexical macro or function bindings.  It must be able, when walking a
;;; macrolet, flet or labels form to construct an environment which reflects
;;; the bindings created by that form.  Note that the environment created
;;; does NOT have to be sufficient to evaluate the body, merely to walk its
;;; body.  This means that definitions do not have to be supplied for lexical
;;; functions, only the fact that that function is bound is important.  For
;;; macros, the macroexpansion function must be supplied.
;;;
;;; This code is organized in a way that lets it work in implementations that
;;; stack cons their environments.  That is reflected in the fact that the
;;; only operation that lets a user build a new environment is a with-body
;;; macro which executes its body with the specified symbol bound to the new
;;; environment.  No code in this walker or in PCL will hold a pointer to
;;; these environments after the body returns.  Other user code is free to do
;;; so in implementations where it works, but that code is not considered
;;; portable.
;;;
;;; There are 3 environment hacking tools.  One macro which is used for
;;; creating new environments, and two functions which are used to access the
;;; bindings of existing environments.
;;;
;;; WITH-AUGMENTED-ENVIRONMENT
;;;
;;; ENVIRONMENT-FUNCTION
;;;
;;; ENVIRONMENT-MACRO
;;; 

(defun unbound-lexical-function (&rest args)
  (declare (ignore args))
  (error "The evaluator was called to evaluate a form in a macroexpansion~%~
          environment constructed by the PCL portable code walker.  These~%~
          environments are only useful for macroexpansion, they cannot be~%~
          used for evaluation.~%~
          This error should never occur when using PCL.~%~
          This most likely source of this error is a program which tries to~%~
          to use the PCL portable code walker to build its own evaluator."))


;;;
;;; In Kyoto Common Lisp, the macroexpansion environment is a three element
;;; list.  The second element describes lexical functions and macros.  The 
;;; function entries in this list have the form 
;;;     (<name> FUNCTION <function-value>)
;;; The macro entries have the form
;;;     (<name> MACRO <macro-value>).
;;;
;;;

(defmacro with-augmented-environment
	  ((new-env old-env &key functions macros) &body body)
	  `(let ((,new-env (with-augmented-environment-internal ,old-env
								,functions
								,macros)))
	     ,@body))

(defun with-augmented-environment-internal (env functions macros)
  (let* ((vars (car env))
	 (funs (cdr env)))
    (dolist (f functions)
      (push `(,(car f) function ,#'unbound-lexical-function) funs))
    (dolist (m macros)
      (push `(,(car m) macro ,(second m)) funs))
    (cons vars funs)))

#+nil
(defun environment-function (env fn)
  (when env
	(let ((entry (assoc fn (cdr env))))
	  (and entry
	       (eq (second entry) 'FUNCTION)
	       (third entry)))))

(defun environment-macro (env macro)
  (declare (si::c-local))
  (when env
	(let ((entry (assoc macro (cdr env))))
	  (and entry
	       (eq (second entry) 'MACRO)
	       (third entry)))))



(defmacro with-new-definition-in-environment
	  ((new-env old-env macrolet/flet/labels-form) &body body)
  (let* ((functions (make-symbol "Functions"))
	 (macros (make-symbol "Macros")))
    `(let ((,functions ())
	   (,macros ()))
       (ecase (car ,macrolet/flet/labels-form)
	 ((FLET LABELS)
	  (dolist (fn (second ,macrolet/flet/labels-form))
	    (push fn ,functions)))
	 ((MACROLET)
	  (dolist (mac (second ,macrolet/flet/labels-form))
	    (push (list (car mac)
			(convert-macro-to-lambda (second mac)
						 (cddr mac)
						 (string (car mac))))
		  ,macros))))
       (with-augmented-environment
	      (,new-env ,old-env :functions ,functions :macros ,macros)
	 ,@body))))

(defun convert-macro-to-lambda (llist body &optional (name "Dummy Macro"))
  (declare (si::c-local))
  (let ((gensym (make-symbol name)))
    (eval `(defmacro ,gensym ,llist ,@body))
    (macro-function gensym)))

;;;
;;; Now comes the real walker.
;;;
;;; As the walker walks over the code, it communicates information to itself
;;; about the walk.  This information includes the walk function, variable
;;; bindings, declarations in effect etc.  This information is inherently
;;; lexical, so the walker passes it around in the actual environment the
;;; walker passes to macroexpansion functions.  This is what makes the
;;; nested-walk-form facility work properly.
;;;
(defmacro walker-environment-bind ((var env &rest key-args)
				      &body body)
  `(with-augmented-environment
     (,var ,env :macros (walker-environment-bind-1 ,env ,.key-args))
     .,body))

(defvar *key-to-walker-environment* (gensym))

(defun env-lock (env)
  (declare (si::c-local))
  (environment-macro env *key-to-walker-environment*))

(defun walker-environment-bind-1 (env &key (walk-function nil wfnp)
					   (walk-form nil wfop)
					   (declarations nil decp)
					   (lexical-variables nil lexp))
  (declare (si::c-local))
  (let ((lock (env-lock env)))
    (list
      (list *key-to-walker-environment*
	    (list (if wfnp walk-function     (car lock))
		  (if wfop walk-form         (second lock))
		  (if decp declarations      (third lock))
		  (if lexp lexical-variables (fourth lock)))))))

(defun env-walk-function (env)
  (declare (si::c-local))
  (first (env-lock env)))

(defun env-walk-form (env)
  (declare (si::c-local))
  (second (env-lock env)))

(defun env-declarations (env)
  (declare (si::c-local))
  (third (env-lock env)))

(defun env-lexical-variables (env)
  (declare (si::c-local))
  (fourth (env-lock env)))


(defun note-declaration (declaration env)
  (declare (si::c-local))
  (push declaration (third (env-lock env))))

(defun note-lexical-binding (thing env)
  (push (list thing :LEXICAL-VAR) (fourth (env-lock env))))

(defun VARIABLE-LEXICAL-P (var env)
  (declare (si::c-local))
  (let ((entry (member var (env-lexical-variables env) :key #'car)))
    (when (eq (cadar entry) :LEXICAL-VAR)
      entry)))

(defun variable-symbol-macro-p (var env)
  (declare (si::c-local))
  (let ((entry (member var (env-lexical-variables env) :key #'car)))
    (when (eq (cadar entry) :macro)
      entry)))

(defvar *VARIABLE-DECLARATIONS* '(SPECIAL TYPE)) ; Beppe

(defun VARIABLE-DECLARATION (declaration var env)
  (if (not (member declaration *variable-declarations*))
      (error "~S is not a recognized variable declaration." declaration)
      (let ((id (or (variable-lexical-p var env) var)))
	(dolist (decl (env-declarations env))
	  (when (and (eq (car decl) declaration)
		     (or (eq (second decl) id)
			 (and (eq 'TYPE (car decl))
			      (member var (cddr decl) :test #'eq)))) ; Beppe
	    (return decl))))))

(defun VARIABLE-SPECIAL-P (var env)
  (or (not (null (variable-declaration 'SPECIAL var env)))
      (variable-globally-special-p var)))

;;;
;;; VARIABLE-GLOBALLY-SPECIAL-P is used to ask if a variable has been
;;; declared globally special.  Any particular CommonLisp implementation
;;; should customize this function accordingly and send their customization
;;; back.
;;;
;;; The default version of variable-globally-special-p is probably pretty
;;; slow, so it uses *globally-special-variables* as a cache to remember
;;; variables that it has already figured out are globally special.
;;;
;;; This would need to be reworked if an unspecial declaration got added to
;;; Common Lisp.
;;;
;;; Common Lisp nit:
;;;   variable-globally-special-p should be defined in Common Lisp.
;;;

(defun variable-globally-special-p (symbol) (si:specialp symbol))


  ;;   
;;;;;; Handling of special forms (the infamous 24).
  ;;
;;;
;;; and I quote...
;;; 
;;;     The set of special forms is purposely kept very small because
;;;     any program analyzing program (read code walker) must have
;;;     special knowledge about every type of special form. Such a
;;;     program needs no special knowledge about macros...
;;;
;;; So all we have to do here is a define a way to store and retrieve
;;; templates which describe how to walk the 24 special forms and we are all
;;; set...
;;;
;;; Well, its a nice concept, and I have to admit to being naive enough that
;;; I believed it for a while, but not everyone takes having only 24 special
;;; forms as seriously as might be nice.  There are (at least) 3 ways to
;;; lose:
;;
;;;   1 - Implementation x implements a Common Lisp special form as a macro
;;;       which expands into a special form which:
;;;         - Is a common lisp special form (not likely)
;;;         - Is not a common lisp special form (on the 3600 IF --> COND).
;;;
;;;     * We can safe ourselves from this case (second subcase really) by
;;;       checking to see if there is a template defined for something
;;;       before we check to see if we we can macroexpand it.
;;;
;;;   2 - Implementation x implements a Common Lisp macro as a special form.
;;;
;;;     * This is a screw, but not so bad, we save ourselves from it by
;;;       defining extra templates for the macros which are *likely* to
;;;       be implemented as special forms.  (DO, DO* ...)
;;;
;;;   3 - Implementation x has a special form which is not on the list of
;;;       Common Lisp special forms.
;;;
;;;     * This is a bad sort of a screw and happens more than I would like
;;;       to think, especially in the implementations which provide more
;;;       than just Common Lisp (3600, Xerox etc.).
;;;       The fix is not terribly staisfactory, but will have to do for
;;;       now.  There is a hook in get walker-template which can get a
;;;       template from the implementation's own walker.  That template
;;;       has to be converted, and so it may be that the right way to do
;;;       this would actually be for that implementation to provide an
;;;       interface to its walker which looks like the interface to this
;;;       walker.
;;;

(eval-when (#-cross compile load eval)

(defmacro get-walker-template-internal (x) ;Has to be inside eval-when because
  `(get-sysprop ,x 'WALKER-TEMPLATE))	   ;Golden Common Lisp doesn't hack
					   ;compile time definition of macros
					   ;right for setf.

(defmacro define-walker-template
	  (name &optional (template '(NIL REPEAT (EVAL))))
  `(eval-when (load eval)
     (put-sysprop ',name 'WALKER-TEMPLATE ',template)))
)

(defun get-walker-template (x)
  (cond ((symbolp x)
	 (or (get-walker-template-internal x)
	     (get-implementation-dependent-walker-template x)))
	((and (listp x) (eq (car x) 'LAMBDA))
	 '(LAMBDA REPEAT (EVAL)))
	(t
	 (error "Can't get template for ~S" x))))

(defun get-implementation-dependent-walker-template (x)
  (declare (ignore x))
  ())


  ;;   
;;;;;; The actual templates
  ;;   

(define-walker-template BLOCK                (NIL NIL REPEAT (EVAL)))
(define-walker-template CATCH                (NIL EVAL REPEAT (EVAL)))
(define-walker-template COMPILER-LET         walk-compiler-let)
(define-walker-template DECLARE              walk-unexpected-declare)
(define-walker-template EVAL-WHEN            (NIL QUOTE REPEAT (EVAL)))
(define-walker-template FLET                 walk-flet)
(define-walker-template FUNCTION             (NIL CALL))
(define-walker-template GO                   (NIL QUOTE))
(define-walker-template IF                   walk-if)
(define-walker-template LABELS               walk-labels)
(define-walker-template LAMBDA               walk-lambda)
(define-walker-template LET                  walk-let)
(define-walker-template LET*                 walk-let*)
(define-walker-template LOCALLY              walk-locally)
(define-walker-template MACROLET             walk-macrolet)
(define-walker-template MULTIPLE-VALUE-CALL  (NIL EVAL REPEAT (EVAL)))
(define-walker-template MULTIPLE-VALUE-PROG1 (NIL RETURN REPEAT (EVAL)))
(define-walker-template MULTIPLE-VALUE-SETQ  walk-multiple-value-setq)
(define-walker-template MULTIPLE-VALUE-BIND  walk-multiple-value-bind)
(define-walker-template PROGN                (NIL REPEAT (EVAL)))
(define-walker-template PROGV                (NIL EVAL EVAL REPEAT (EVAL)))
(define-walker-template QUOTE                (NIL QUOTE))
(define-walker-template RETURN-FROM          (NIL QUOTE REPEAT (RETURN)))
(define-walker-template SETQ                 walk-setq)
(define-walker-template SYMBOL-MACROLET      walk-symbol-macrolet)
(define-walker-template TAGBODY              walk-tagbody)
(define-walker-template THE                  (NIL QUOTE EVAL))
(define-walker-template THROW                (NIL EVAL EVAL))
(define-walker-template UNWIND-PROTECT       (NIL RETURN REPEAT (EVAL)))

;;; The new special form.
;(define-walker-template pcl::LOAD-TIME-EVAL       (NIL EVAL))

;;;
;;; And the extra templates...
;;;
#+ecl
(define-walker-template DOTIMES	walk-dotimes/dolist)
#+ecl
(define-walker-template DOLIST	walk-dotimes/dolist)
#+ecl
(define-walker-template WHEN	walk-when/unless)
#+ecl
(define-walker-template UNLESS	walk-when/unless)
(define-walker-template DO      walk-do)
(define-walker-template DO*     walk-do*)
(define-walker-template PROG    walk-prog)
(define-walker-template PROG*   walk-prog*)
(define-walker-template COND    (NIL REPEAT ((TEST REPEAT (EVAL)))))
(define-walker-template ext::lambda-block walk-named-lambda)	;Not really right, but
							        ;we don't hack block
						        	;names anyways.
#+ecl
(define-walker-template ffi::c-inline walk-c-inline)


;;; Controls whether macros are expanded by walk-form
(defvar WALK-FORM-EXPAND-MACROS-P nil)

(defun macroexpand-all (form &optional environment)
  (let ((walk-form-expand-macros-p t))
    (walk-form form environment)))

(defun WALK-FORM (form
		  &optional environment
			    (walk-function
			      #'(lambda (subform context env)
				  (declare (ignore context env))
				  subform)))
  (walker-environment-bind (new-env environment :walk-function walk-function)
    (walk-form-internal form :eval new-env)))

;;;
;;; nested-walk-form provides an interface that allows nested macros, each
;;; of which must walk their body to just do one walk of the body of the
;;; inner macro.  That inner walk is done with a walk function which is the
;;; composition of the two walk functions.
;;;
;;; This facility works by having the walker annotate the environment that
;;; it passes to macroexpand-1 to know which form is being macroexpanded.
;;; If then the &whole argument to the macroexpansion function is eq to
;;; the env-walk-form of the environment, nested-walk-form can be certain
;;; that there are no intervening layers and that a nested walk is alright.
;;;
;;; There are some semantic problems with this facility.  In particular, if
;;; the outer walk function returns T as its walk-no-more-p value, this will
;;; prevent the inner walk function from getting a chance to walk the subforms
;;; of the form.  This is almost never what you want, since it destroys the
;;; equivalence between this nested-walk-form function and two seperate
;;; walk-forms.
;;;
#-ecl
(defun NESTED-WALK-FORM (whole
			 form
			 &optional environment
				   (walk-function
				     #'(lambda (subform context env)
					 (declare (ignore context env))
					 subform)))
  (if (eq whole (env-walk-form environment))
      (let ((outer-walk-function (env-walk-function environment)))
	(throw whole
	  (walk-form
	    form
	    environment
	    #'(lambda (f c e)
		;; First loop to make sure the inner walk function
		;; has done all it wants to do with this form.
		;; Basically, what we are doing here is providing
		;; the same contract walk-form-internal normally
		;; provides to the inner walk function.
		(let*((inner-result nil)
		      (inner-no-more-p nil)
		      (outer-result nil)
		      (outer-no-more-p nil))
		  (loop
		    (multiple-value-setq (inner-result inner-no-more-p)
					 (funcall walk-function f c e))
		    (cond (inner-no-more-p (return))
			  ((not (eq inner-result f)))
			  ((not (consp inner-result)) (return))
			  ((get-walker-template (car inner-result)) (return))
			  (t
			   (multiple-value-bind (expansion macrop)
			       (walker-environment-bind
				     (new-env e :walk-form inner-result)
				 (macroexpand-1 inner-result new-env))
			     (if macrop
				 (setq inner-result expansion)
				 (return)))))
		    (setq f inner-result))
		  (multiple-value-setq (outer-result outer-no-more-p)
				       (funcall outer-walk-function
						inner-result
						c
						e))
		  (values outer-result
			  (and inner-no-more-p outer-no-more-p)))))))
      (walk-form form environment walk-function)))

;;;
;;; WALK-FORM-INTERNAL is the main driving function for the code walker. It
;;; takes a form and the current context and walks the form calling itself or
;;; the appropriate template recursively.
;;;
;;;   "It is recommended that a program-analyzing-program process a form
;;;    that is a list whose car is a symbol as follows:
;;;
;;;     1. If the program has particular knowledge about the symbol,
;;;        process the form using special-purpose code.  All of the
;;;        standard special forms should fall into this category.
;;;     2. Otherwise, if macro-function is true of the symbol apply
;;;        either macroexpand or macroexpand-1 and start over.
;;;     3. Otherwise, assume it is a function call. "
;;;     

(defun walk-form-internal (form context env
			   &aux fn template)
  ;; First apply the walk-function to perform whatever translation
  ;; the user wants to this form.  If the second value returned
  ;; by walk-function is T then we don't recurse...
  (catch form
    (multiple-value-bind (newform walk-no-more-p)
      (funcall (env-walk-function env) form context env)
      (catch newform
	(cond (walk-no-more-p newform)
	      ((not (eq form newform))
	       (walk-form-internal newform context env))
	      ((not (consp newform))
	       (let ((symmac (car (variable-symbol-macro-p newform env))))
		 (if symmac
		     (let ((newnewform (walk-form-internal (cddr symmac)
							   context env)))
		       (if (eq newnewform (cddr symmac))
			   (if walk-form-expand-macros-p newnewform newform)
			   newnewform))
		     newform)))
	      ((setq template (get-walker-template (setq fn (car newform))))
	       (if (symbolp template)
		   (funcall template newform context env)
		   (walk-template newform template context env)))
	      (t
	       (multiple-value-bind (newnewform macrop)
		 (walker-environment-bind (new-env env :walk-form newform)
					  (macroexpand-1 newform new-env))
		 (cond
		   (macrop
		    (let ((newnewnewform
			   (walk-form-internal newnewform context env)))
		      (if (eq newnewnewform newnewform)
			  (if walk-form-expand-macros-p newnewform newform)
			  newnewnewform)))
		   ((and (symbolp fn)
			 (not (fboundp fn))
			 (special-operator-p fn))
		    (error
		     "~S is a special form, not defined in the CommonLisp.~%~
                       manual This code walker doesn't know how to walk it.~%~
                       Define a template for this special form and try again."
		     fn))
		   (t
		    ;; Otherwise, walk the form as if its just a standard 
		    ;; functioncall using a template for standard function
		    ;; call.
		    (walk-template
		     newnewform '(CALL REPEAT (EVAL)) context env))))))))))

(defun walk-template (form template context env)
  (declare (si::c-local))
  (if (atom template)
      (ecase template
        ((EVAL FUNCTION TEST EFFECT RETURN)
         (walk-form-internal form :EVAL env))
        ((QUOTE NIL) form)
        (SET
          (walk-form-internal form :SET env))
        ((LAMBDA CALL)
	 (cond ((or (symbolp form)
		    (and (listp form)
			 (= (length form) 2)
			 (eq (car form) 'SETF))) form)
	       (t (walk-form-internal form context env)))))
      (case (car template)
        (REPEAT
          (walk-template-handle-repeat form
                                       (cdr template)
				       ;; For the case where nothing happens
				       ;; after the repeat optimize out the
				       ;; call to length.
				       (if (null (cddr template))
					   ()
					   (nthcdr (- (length form)
						      (length
							(cddr template)))
						   form))
                                       context
				       env))
        (IF
	  (walk-template form
			 (if (if (listp (second template))
				 (eval (second template))
				 (funcall (second template) form))
			     (third template)
			     (fourth template))
			 context
			 env))
        (REMOTE
          (walk-template form (second template) context env))
        (otherwise
          (cond ((atom form) form)
                (t (recons form
                           (walk-template
			     (car form) (car template) context env)
                           (walk-template
			     (cdr form) (cdr template) context env))))))))

(defun walk-template-handle-repeat (form template stop-form context env)
  (declare (si::c-local))
  (if (eq form stop-form)
      (walk-template form (cdr template) context env)
      (walk-template-handle-repeat-1 form
				     template
				     (car template)
				     stop-form
				     context
				     env)))

(defun walk-template-handle-repeat-1 (form template repeat-template
					   stop-form context env)
  (declare (si::c-local))
  (cond ((null form) ())
        ((eq form stop-form)
         (if (null repeat-template)
             (walk-template stop-form (cdr template) context env)       
             (error "While handling repeat:~%~
                     Ran into stop while still in repeat template.")))
        ((null repeat-template)
         (walk-template-handle-repeat-1
	   form template (car template) stop-form context env))
        (t
         (recons form
                 (walk-template (car form) (car repeat-template) context env)
                 (walk-template-handle-repeat-1 (cdr form)
						template
						(cdr repeat-template)
						stop-form
						context
						env)))))

(defun walk-repeat-eval (form env)
  (and form
       (recons form
	       (walk-form-internal (car form) :eval env)
	       (walk-repeat-eval (cdr form) env))))

(defun recons (x car cdr)
  (if (or (not (eq (car x) car))
          (not (eq (cdr x) cdr)))
      (cons car cdr)
      x))

(defun relist (x &rest args)
  (declare (si::c-local))
  (if (null args)
      nil
      (relist-internal x args nil)))

(defun relist* (x &rest args)
  (declare (si::c-local))
  (relist-internal x args 'T))

(defun relist-internal (x args *p)
  (declare (si::c-local))
  (if (null (cdr args))
      (if *p (car args) (recons x (car args) nil))
      (recons x
	      (car args)
	      (relist-internal (cdr x) (cdr args) *p))))


  ;;   
;;;;;; Special walkers
  ;;

(defun walk-declarations (body fn env
			       &optional doc-string-p declarations old-body
			       &aux (form (car body)) macrop new-form)
  (declare (si::c-local))
  (cond ((and (stringp form)			;might be a doc string
              (cdr body)			;isn't the returned value
              (null doc-string-p)		;no doc string yet
              (null declarations))		;no declarations yet
         (recons body
                 form
                 (walk-declarations (cdr body) fn env t)))
        ((and (listp form) (eq (car form) 'DECLARE))
         ;; Got ourselves a real live declaration.  Record it, look for more.
         (dolist (declaration (cdr form))
	   (let*((type (car declaration))
		 (name (second declaration))
		 (args (cddr declaration)))
	     (if (member type *variable-declarations*)
		 (note-declaration `(,type
				     ,(or (variable-lexical-p name env) name)
				     ,.args)
				   env)
		 (note-declaration declaration env))
	     (push declaration declarations)))
         (recons body
                 form
                 (walk-declarations
		   (cdr body) fn env doc-string-p declarations)))
        ((and form
	      (listp form)
	      (null (get-walker-template (car form)))
	      (progn
		(multiple-value-setq (new-form macrop)
				     (macroexpand-1 form env))
		macrop))
	 ;; This form was a call to a macro.  Maybe it expanded
	 ;; into a declare?  Recurse to find out.
	 (walk-declarations (recons body new-form (cdr body))
			    fn env doc-string-p declarations
			    (or old-body body)))
	(t
	 ;; Now that we have walked and recorded the declarations,
	 ;; call the function our caller provided to expand the body.
	 ;; We call that function rather than passing the real-body
	 ;; back, because we are RECONSING up the new body.
	 (funcall fn (or old-body body) env))))


(defun walk-unexpected-declare (form context env)
  (declare (ignore context env)
	   (si::c-local))
  (warn "Encountered declare ~S in a place where a declare was not expected."
	form)
  form)

(defun walk-arglist (arglist context env &optional (destructuringp nil)
					 &aux arg)
  (declare (si::c-local))
  (cond ((null arglist) ())
        ((symbolp (setq arg (car arglist)))
         (or (member arg lambda-list-keywords)
             (note-lexical-binding arg env))
         (recons arglist
                 arg
                 (walk-arglist (cdr arglist)
                               context
			       env
                               (and destructuringp
				    (not (member arg
						 lambda-list-keywords))))))
        ((consp arg)
         (prog1 (recons arglist
			(if destructuringp
			    (walk-arglist arg context env destructuringp)
			    (relist* arg
				     (car arg)
				     (walk-form-internal (second arg) :eval env)
				     (cddr arg)))
				    (walk-arglist (cdr arglist) context env nil))
                (if (symbolp (car arg))
                    (note-lexical-binding (car arg) env)
                    (note-lexical-binding (cadar arg) env))
                (or (null (cddr arg))
                    (not (symbolp (third arg)))
                    (note-lexical-binding (third arg) env))))
          (t
	   (error "Can't understand something in the arglist ~S" arglist))))

(defun walk-let (form context env)
  (walk-let/let* form context env nil))

(defun walk-let* (form context env)
  (walk-let/let* form context env t))

(defun walk-prog (form context env)
  (walk-prog/prog* form context env nil))

(defun walk-prog* (form context env)
  (walk-prog/prog* form context env t))

(defun walk-do (form context env)
  (walk-do/do* form context env nil))

(defun walk-do* (form context env)
  (walk-do/do* form context env t))

(defun walk-let/let* (form context old-env sequentialp)
  (walker-environment-bind (new-env old-env)
    (let* ((let/let* (car form))
	   (bindings (second form))
	   (body (cddr form))
	   (walked-bindings 
	     (walk-bindings-1 bindings
			      old-env
			      new-env
			      context
			      sequentialp))
	   (walked-body
	     (walk-declarations body #'walk-repeat-eval new-env)))
      (relist*
	form let/let* walked-bindings walked-body))))

(defun walk-locally (form context env)
  (declare (ignore context))
  (let* ((locally (car form))
	 (body (cdr form))
	 (walked-body
	  (walk-declarations body #'walk-repeat-eval env)))
    (relist*
     form locally walked-body)))

(defun walk-prog/prog* (form context old-env sequentialp)
  (walker-environment-bind (new-env old-env)
    (let* ((possible-block-name (second form))
	   (blocked-prog (and (symbolp possible-block-name)
			      (not (eq possible-block-name 'nil)))))
      (multiple-value-bind (let/let* block-name bindings body)
	  (if blocked-prog
	      (values (car form) (cadr form) (caddr form) (cdddr form))
	      (values (car form) nil	     (cadr  form) (cddr  form)))
	(let* ((walked-bindings 
		 (walk-bindings-1 bindings
				  old-env
				  new-env
				  context
				  sequentialp))
	       (walked-body
		 (walk-declarations 
		   body
		   #'(lambda (real-body real-env)
		       (walk-tagbody-1 real-body context real-env))
		   new-env)))
	  (if block-name
	      (relist*
		form let/let* block-name walked-bindings walked-body)
	      (relist*
		form let/let* walked-bindings walked-body)))))))

(defun walk-do/do* (form context old-env sequentialp)
  (walker-environment-bind (new-env old-env)
    (let* ((do/do* (car form))
	   (bindings (second form))
	   (end-test (third form))
	   (body (cdddr form))
	   (walked-bindings (walk-bindings-1 bindings
					     old-env
					     new-env
					     context
					     sequentialp))
	   (walked-body
	     (walk-declarations body #'walk-repeat-eval new-env)))
      (relist* form
	       do/do*
	       (walk-bindings-2 bindings walked-bindings context new-env)
	       (walk-template end-test '(TEST REPEAT (EVAL)) context new-env)
	       walked-body))))

#+ecl
(defun walk-dotimes/dolist (form context old-env)
  (walker-environment-bind (new-env old-env)
    (let* ((dotimes/dolist (car form))
	   (bindings (second form))
	   (body (cddr form))
	   ; This is a hack. We tread BINDINGS as we
	   ; would in a DO/DO* loop.
	   (walked-bindings (walk-bindings-1 bindings
					     old-env
					     new-env
					     context
					     t))
	   (walked-body
	     (walk-declarations body #'walk-repeat-eval new-env)))
      (relist* form
	       dotimes/dolist
	       (walk-bindings-2 bindings walked-bindings context new-env)
	       walked-body))))

(defun walk-multiple-value-setq (form context env)
  (let ((vars (cadr form)))
    (if (some #'(lambda (var)
		  (variable-symbol-macro-p var env))
	      vars)
	(let* ((temps (mapcar #'(lambda (var) (declare (ignore var)) (gensym)) vars))
	       (sets (mapcar #'(lambda (var temp) `(setq ,var ,temp)) vars temps))
	       (expanded `(multiple-value-bind ,temps 
			       ,(caddr form)
			     ,@sets))
	       (walked (walk-form-internal expanded context env)))
	  (if (eq walked expanded)
	      form
	      walked))
	(walk-template form '(nil (repeat (set)) eval) context env))))

(defun walk-multiple-value-bind (form context old-env)
  (walker-environment-bind (new-env old-env)
    (let* ((mvb (car form))
	   (bindings (second form))
	   (mv-form (walk-template (third form) 'EVAL context old-env))
	   (body (cdddr form))
	   walked-bindings
	   (walked-body
	     (walk-declarations 
	       body
	       #'(lambda (real-body real-env)
		   (setq walked-bindings
			 (walk-bindings-1 bindings
					  old-env
					  new-env
					  context
					  nil))
		   (walk-repeat-eval real-body real-env))
	       new-env)))
      (relist* form mvb walked-bindings mv-form walked-body))))

(defun walk-bindings-1 (bindings old-env new-env context sequentialp)
  (and bindings
       (let ((binding (car bindings)))
         (recons bindings
                 (if (symbolp binding)
                     (prog1 binding
                            (note-lexical-binding binding new-env))
                     (prog1 (relist* binding
				     (car binding)
				     (walk-form-internal (second binding)
							 context
							 (if sequentialp
							     new-env
							     old-env))
				     (cddr binding))	;save cddr for DO/DO*
						        ;it is the next value
						        ;form. Don't walk it
						        ;now though.
                            (note-lexical-binding (car binding) new-env)))
                 (walk-bindings-1 (cdr bindings)
				  old-env
				  new-env
				  context
				  sequentialp)))))

(defun walk-bindings-2 (bindings walked-bindings context env)
  (declare (si::c-local))
  (and bindings
       (let ((binding (car bindings))
             (walked-binding (car walked-bindings)))
         (recons bindings
		 (if (symbolp binding)
		     binding
		     (relist* binding
			      (car walked-binding)
			      (second walked-binding)
			      (walk-template (cddr binding)
					     '(EVAL)
					     context
					     env)))		 
                 (walk-bindings-2 (cdr bindings)
				  (cdr walked-bindings)
				  context
				  env)))))

(defun walk-lambda (form context old-env)
  (walker-environment-bind (new-env old-env)
    (let* ((arglist (second form))
           (body (cddr form))
           (walked-arglist (walk-arglist arglist context new-env))
           (walked-body
             (walk-declarations body #'walk-repeat-eval new-env)))
      (relist* form
               (car form)
	       walked-arglist
               walked-body))))

(defun walk-named-lambda (form context old-env)
  (walker-environment-bind (new-env old-env)
    (let* ((name (second form))
	   (arglist (third form))
           (body (cdddr form))
           (walked-arglist (walk-arglist arglist context new-env))
           (walked-body
             (walk-declarations body #'walk-repeat-eval new-env)))
      (relist* form
               (car form)
	       name
	       walked-arglist
               walked-body))))  

(defun walk-setq (form context env)
  (if (cdddr form)
      (let* ((expanded (let* ((rforms nil)
			      (tail (cdr form)))
			 (loop (when (null tail) (return (nreverse rforms)))
			       (let ((var (pop tail)) (val (pop tail)))
				 (push `(setq ,var ,val) rforms)))))
	     (walked (walk-repeat-eval expanded env)))
	(if (eq expanded walked)
	    form
	    `(progn ,@walked)))
      (let* ((var (cadr form))
	     (val (caddr form))
	     (symmac (car (variable-symbol-macro-p var env))))
	(if symmac
	    (let* ((expanded `(setf ,(cddr symmac) ,val))
		   (walked (walk-form-internal expanded context env)))
	      (if (eq expanded walked)
		  form
		  walked))
	    (relist form 'setq
		    (walk-form-internal var :set env)
		    (walk-form-internal val :eval env))))))

(defun walk-symbol-macrolet (form context old-env)
  (declare (ignore context))
  (let* ((bindings (second form)))
    (walker-environment-bind
	(new-env old-env
		 :lexical-variables
		 (append (mapcar #'(lambda (binding)
				     `(,(first binding)
				       :macro . ,(second binding)))
				 bindings)
			 (env-lexical-variables old-env)))
      (relist* form 'SYMBOL-MACROLET bindings
	       (walk-repeat-eval (cddr form) new-env)))))

(defun walk-tagbody (form context env)
  (recons form (car form) (walk-tagbody-1 (cdr form) context env)))

(defun walk-tagbody-1 (form context env)
  (declare (si::c-local))
  (and form
       (recons form
               (walk-form-internal (car form)
				   (if (symbolp (car form)) 'QUOTE context)
				   env)
               (walk-tagbody-1 (cdr form) context env))))

(defun walk-compiler-let (form context old-env)
  (declare (ignore context))
  (let* ((vars ())
	 (vals ()))
    (dolist (binding (second form))
      (cond ((symbolp binding) (push binding vars) (push nil vals))
	    (t
	     (push (car binding) vars)
	     (push (eval (second binding)) vals))))
    (relist* form
	     (car form)
	     (second form)
	     (progv vars vals (walk-repeat-eval (cddr form) old-env)))))

(defun walk-macrolet (form context old-env)
  (walker-environment-bind (macro-env
			    nil
			    :walk-function (env-walk-function old-env))
    (labels ((walk-definitions (definitions)
	       (and definitions
		    (let ((definition (car definitions)))
		      (recons definitions
                              (relist* definition
                                       (car definition)
                                       (walk-arglist (second definition)
						     context
						     macro-env
						     t)
                                       (walk-declarations (cddr definition)
							  #'walk-repeat-eval
							  macro-env))
			      (walk-definitions (cdr definitions)))))))
      (with-new-definition-in-environment (new-env old-env form)
	(relist* form
		 (car form)
		 (walk-definitions (second form))
		 (walk-declarations (cddr form)
				    #'walk-repeat-eval
				    new-env))))))

(defun walk-flet (form context old-env)
  (labels ((walk-definitions (definitions)
	     (if (null definitions)
		 ()
		 (recons definitions
			 (walk-lambda (car definitions) context old-env)
			 (walk-definitions (cdr definitions))))))
    (recons form
	    (car form)
	    (recons (cdr form)
		    (walk-definitions (second form))
		    (with-new-definition-in-environment (new-env old-env form)
		      (walk-declarations (cddr form)
					 #'walk-repeat-eval
					 new-env))))))

(defun walk-labels (form context old-env)
  (with-new-definition-in-environment (new-env old-env form)
    (labels ((walk-definitions (definitions)
	       (if (null definitions)
		   ()
		   (recons definitions
			   (walk-lambda (car definitions) context new-env)
			   (walk-definitions (cdr definitions))))))
      (recons form
	      (car form)
	      (recons (cdr form)
		      (walk-definitions (second form))
		      (walk-declarations (cddr form)
					 #'walk-repeat-eval
					 new-env))))))

(defun walk-if (form context env)
  (let*((predicate (second form))
	(arm1 (third form))
	(arm2 
	  (if (cddddr form)
	      (progn
		(warn "In the form:~%~S~%~
                       IF only accepts three arguments, you are using ~D.~%~
                       It is true that some Common Lisps support this, but ~
                       it is not~%~
                       truly legal Common Lisp.  For now, this code ~
                       walker is interpreting ~%~
                       the extra arguments as extra else clauses. ~
                       Even if this is what~%~
                       you intended, you should fix your source code."
		      form
		      (length (cdr form)))
		(cons 'PROGN (cdddr form)))
	      (fourth form))))
    (relist form
	    'IF
	    (walk-form-internal predicate context env)
	    (walk-form-internal arm1 context env)
	    (walk-form-internal arm2 context env))))

#+ecl
(defun walk-when/unless (form context env)
  (relist* form
	   (first form)
	   (walk-form-internal (second form) context env) ; predicate
	   (walk-repeat-eval (cddr form) env)))

#+ecl
(defun walk-c-inline (form context env)
  (relist* form
	   (first form)
	   (walk-repeat-eval (second form) env) ; arguments
	   (cddr form))) ; types and flags of the form

;;;
;;; Tests tests tests
;;;

#|
;;; 
;;; Here are some examples of the kinds of things you should be able to do
;;; with your implementation of the macroexpansion environment hacking
;;; mechanism.
;;; 
;;; with-lexical-macros is kind of like macrolet, but it only takes names
;;; of the macros and actual macroexpansion functions to use to macroexpand
;;; them.  The win about that is that for macros which want to wrap several
;;; macrolets around their body, they can do this but have the macroexpansion
;;; functions be compiled.  See the WITH-RPUSH example.
;;;
;;; If the implementation had a special way of communicating the augmented
;;; environment back to the evaluator that would be totally great.  It would
;;; mean that we could just augment the environment then pass control back
;;; to the implementations own compiler or interpreter.  We wouldn't have
;;; to call the actual walker.  That would make this much faster.  Since the
;;; principal client of this is defmethod it would make compiling defmethods
;;; faster and that would certainly be a win.
;;;
(defmacro with-lexical-macros (macros &body body &environment old-env)
  (with-augmented-environment (new-env old-env :macros macros)
    (walk-form (cons 'PROGN body) new-env)))

(defun expand-rpush (form env)
  `(push ,(third form) ,(second form)))

(defmacro with-rpush (&body body)
  `(with-lexical-macros ,(list (list 'RPUSH #'expand-rpush)) ,@body))


;;;
;;; Unfortunately, I don't have an automatic tester for the walker.  
;;; Instead there is this set of test cases with a description of
;;; how each one should go.
;;; 
(defmacro take-it-out-for-a-test-walk (form)
  `(progn 
     (terpri)
     (terpri)
     (let ((copy-of-form (copy-tree ',form))
           (result (walk-form ',form nil
                              '(lambda (x y env)
                                 (format t "~&Form: ~S ~3T Context: ~A" x y)
                                 (when (symbolp x)
				   (let ((lexical (variable-lexical-p x env))
					 (special (variable-special-p x env)))
                                     (when lexical
                                       (format t ";~3T")
                                       (format t "lexically bound"))
                                     (when special
                                       (format t ";~3T")
                                       (format t "declared special"))
                                     (when (boundp x)
                                       (format t ";~3T")
                                       (format t "bound: ~S " (eval x)))))
                                 x))))
       (cond ((not (equal result copy-of-form))
              (format t "~%Warning: Result not EQUAL to copy of start."))
             ((not (eq result ',form))
              (format t "~%Warning: Result not EQ to copy of start.")))
       (pprint result)
       result)))

(defmacro foo (&rest ignore) ''GLOBAL-FOO)

(defmacro bar (&rest ignore) ''GLOBAL-BAR)

(take-it-out-for-a-test-walk (list arg1 arg2 arg3))
(take-it-out-for-a-test-walk (list (cons 1 2) (list 3 4 5)))

(take-it-out-for-a-test-walk (progn (foo) (bar 1)))

(take-it-out-for-a-test-walk (block block-name a b c))
(take-it-out-for-a-test-walk (block block-name (list a) b c))

(take-it-out-for-a-test-walk (catch catch-tag (list a) b c))
;;;
;;; This is a fairly simple macrolet case.  While walking the body of the
;;; macro, x should be lexically bound. In the body of the macrolet form
;;; itself, x should not be bound.
;;; 
(take-it-out-for-a-test-walk
  (macrolet ((foo (x) (list x) ''INNER))
    x
    (foo 1)))

;;;
;;; A slightly more complex macrolet case.  In the body of the macro x
;;; should not be lexically bound.  In the body of the macrolet form itself
;;; x should be bound.  Note that THIS CASE WILL CAUSE AN ERROR when it
;;; tries to macroexpand the call to foo.
;;; 
(take-it-out-for-a-test-walk
     (let ((x 1))
       (macrolet ((foo () (list x) ''INNER))
	 x
	 (foo))))

;;;
;;; A truly hairy use of compiler-let and macrolet.  In the body of the
;;; macro x should not be lexically bound.  In the body of the macrolet
;;; itself x should not be lexically bound.  But the macro should expand
;;; into 1.
;;; 
(take-it-out-for-a-test-walk
  (compiler-let ((x 1))
    (let ((x 2))
      (macrolet ((foo () x))
	x
	(foo)))))


(take-it-out-for-a-test-walk
  (flet ((foo (x) (list x y))
	 (bar (x) (list x y)))
    (foo 1)))

(take-it-out-for-a-test-walk
  (let ((y 2))
    (flet ((foo (x) (list x y))
	   (bar (x) (list x y)))
      (foo 1))))

(take-it-out-for-a-test-walk
  (labels ((foo (x) (bar x))
	   (bar (x) (foo x)))
    (foo 1)))

(take-it-out-for-a-test-walk
  (flet ((foo (x) (foo x)))
    (foo 1)))

(take-it-out-for-a-test-walk
  (flet ((foo (x) (foo x)))
    (flet ((bar (x) (foo x)))
      (bar 1))))

(take-it-out-for-a-test-walk (compiler-let ((a 1) (b 2)) (foo a) b))
(take-it-out-for-a-test-walk (prog () (declare (special a b))))
(take-it-out-for-a-test-walk (let (a b c)
                               (declare (special a b))
                               (foo a) b c))
(take-it-out-for-a-test-walk (let (a b c)
                               (declare (special a) (special b))
                               (foo a) b c))
(take-it-out-for-a-test-walk (let (a b c)
                               (declare (special a))
                               (declare (special b))
                               (foo a) b c))
(take-it-out-for-a-test-walk (let (a b c)
                               (declare (special a))
                               (declare (special b))
                               (let ((a 1))
                                 (foo a) b c)))
(take-it-out-for-a-test-walk (eval-when ()
                               a
                               (foo a)))
(take-it-out-for-a-test-walk (eval-when (eval when load)
                               a
                               (foo a)))

(take-it-out-for-a-test-walk (multiple-value-bind (a b) (foo a b) (list a b)))
(take-it-out-for-a-test-walk (multiple-value-bind (a b)
				 (foo a b)
			       (declare (special a))
			       (list a b)))
(take-it-out-for-a-test-walk (progn (function foo)))
(take-it-out-for-a-test-walk (progn a b (go a)))
(take-it-out-for-a-test-walk (if a b c))
(take-it-out-for-a-test-walk (if a b))
(take-it-out-for-a-test-walk ((lambda (a b) (list a b)) 1 2))
(take-it-out-for-a-test-walk ((lambda (a b) (declare (special a)) (list a b))
			      1 2))
(take-it-out-for-a-test-walk (let ((a a) (b a) (c b)) (list a b c)))
(take-it-out-for-a-test-walk (let* ((a a) (b a) (c b)) (list a b c)))
(take-it-out-for-a-test-walk (let ((a a) (b a) (c b))
                               (declare (special a b))
                               (list a b c)))
(take-it-out-for-a-test-walk (let* ((a a) (b a) (c b))
                               (declare (special a b))
                               (list a b c)))
(take-it-out-for-a-test-walk (let ((a 1) (b 2))
                               (foo bar)
                               (declare (special a))
                               (foo a b)))
(take-it-out-for-a-test-walk (multiple-value-call #'foo a b c))
(take-it-out-for-a-test-walk (multiple-value-prog1 a b c))
(take-it-out-for-a-test-walk (progn a b c))
(take-it-out-for-a-test-walk (progv vars vals a b c))
(take-it-out-for-a-test-walk (quote a))
(take-it-out-for-a-test-walk (return-from block-name a b c))
(take-it-out-for-a-test-walk (setq a 1))
(take-it-out-for-a-test-walk (setq a (foo 1) b (bar 2) c 3))
(take-it-out-for-a-test-walk (tagbody a b c (go a)))
(take-it-out-for-a-test-walk (the foo (foo-form a b c)))
(take-it-out-for-a-test-walk (throw tag-form a))
(take-it-out-for-a-test-walk (unwind-protect (foo a b) d e f))

(defmacro flet-1 (a b) ''OUTER)
(defmacro labels-1 (a b) ''OUTER)

(take-it-out-for-a-test-walk
  (flet ((flet-1 (a b) () (flet-1 a b) (list a b)))
    (flet-1 1 2)
    (foo 1 2)))
(take-it-out-for-a-test-walk
  (labels ((label-1 (a b) () (label-1 a b)(list a b)))
    (label-1 1 2)
    (foo 1 2)))
(take-it-out-for-a-test-walk (macrolet ((macrolet-1 (a b) (list a b)))
                               (macrolet-1 a b)
                               (foo 1 2)))

(take-it-out-for-a-test-walk (macrolet ((foo (a) `(inner-foo-expanded ,a)))
                               (foo 1)))

(take-it-out-for-a-test-walk (progn (bar 1)
                                    (macrolet ((bar (a)
						 `(inner-bar-expanded ,a)))
                                      (bar 2))))

(take-it-out-for-a-test-walk (progn (bar 1)
                                    (macrolet ((bar (s)
						 (bar s)
						 `(inner-bar-expanded ,s)))
                                      (bar 2))))

(take-it-out-for-a-test-walk (cond (a b)
                                   ((foo bar) a (foo a))))


(let ((the-lexical-variables ()))
  (walk-form '(let ((a 1) (b 2))
		#'(lambda (x) (list a b x y)))
	     ()
	     #'(lambda (form context env)
		 (when (and (symbolp form)
			    (variable-lexical-p form env))
		   (push form the-lexical-variables))
		 form))
  (or (and (= (length the-lexical-variables) 3)
	   (member 'a the-lexical-variables)
	   (member 'b the-lexical-variables)
	   (member 'x the-lexical-variables))
      (error "Walker didn't do lexical variables of a closure properly.")))
    
|#
