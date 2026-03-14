;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CLOS")

(defgeneric ensure-generic-function-using-class
    (generic-function-or-nil function-name &key &allow-other-keys))
;; GENERIC-FUNCTION-NAME is defined earlier in misc.lisp, in case something
;; goes wrong and we want to print generic functions.
(defgeneric (setf generic-function-name) (name generic-function))

(defgeneric add-method (generic-function method))
(defgeneric remove-method (generic-function method))
(defgeneric find-method (generic-function qualifiers specializers &optional errorp))

;;; ----------------------------------------------------------------------
;;; DEFGENERIC
;;;

(defmacro defgeneric (function-specifier lambda-list &rest options)
  (parse-lambda-list lambda-list)
  ;; process options
  (multiple-value-bind (option-list method-list)
      (parse-generic-options options lambda-list)
    (let* ((output `(progn
                      (eval-when (:compile-toplevel)
                        (cmp:register-global-function-def 'defgeneric ',function-specifier))
                      (ensure-generic-function ',function-specifier
                                               :delete-methods t ,@option-list))))
      (if method-list
          `(progn
             ,output
             (associate-methods-to-gfun
              ',function-specifier
              ,@(loop for m in method-list collect `(defmethod ,function-specifier ,@m))))
	  output))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun parse-generic-options (options lambda-list)
  (let* ((processed-options '())
	 (method-list '())
	 (declarations '())
	 arg-list)
    (dolist (option options)
      (ext:with-current-source-form (option)
        (let ((option-name (first option))
              option-value)
          (cond ((eq option-name :method)
                 ;; We do not need to check the validity of this
                 ;; because DEFMETHOD will do it.
                 (push (rest option) method-list))
                ((eq option-name 'declare)
                 (setf declarations (append (rest option) declarations)))
                ((member option-name processed-options)
                 (core:simple-program-error "Option ~s specified more than once"
                                            option-name))
                (t
                 (push option-name processed-options)
                 ;; We leave much of the type checking for SHARED-INITIALIZE
                 (setq option-value
                       (case option-name
                         (:argument-precedence-order
                          (rest option))
                         (:method-combination
                          (rest option))
                         ((:documentation :generic-function-class :method-class)
                          (unless (endp (cddr option))
                            (core:simple-program-error "Too many arguments for option ~A"
                                                       option-name))
                          (second option))
                         (otherwise
                          (core:simple-program-error "~S is not a legal defgeneric option"
                                                     option-name))))
                 (setf arg-list `(',option-name ',option-value ,@arg-list)))))))
    (values `(:lambda-list ',lambda-list ,@arg-list
              ,@(when (ext:current-source-location)
                  (list ''source-position (ext:current-source-location)))
	      ,@(when declarations `(:declarations ',declarations)))
	    method-list)))
)

(eval-when (:compile-toplevel :load-toplevel :execute)
;;; For effect. Checks whether the lambda list is well formed.
(defun parse-lambda-list (lambda-list &optional post-keyword)
  (ext:with-current-source-form (lambda-list)
    (let ((arg (car lambda-list)))
      (cond ((null lambda-list))
            ((eq arg '&AUX)
             (core:simple-program-error "&aux is not allowed in a generic function lambda-list"))
            ((member arg lambda-list-keywords)
             (parse-lambda-list (cdr lambda-list) t))
            (post-keyword
             ;; After a lambda-list-keyword there can be no specializers.
             (parse-lambda-list (cdr lambda-list) t))
            (t
             (if (listp arg)
                 (core:simple-program-error "the parameters cannot be specialized in generic function lambda-list")
                 (parse-lambda-list (cdr lambda-list))))))))
)

(defun valid-declaration-p (decl)
  (and (eq (first decl) 'OPTIMIZE)
       (loop for item in decl
	  always (or (atom item)
		     (and (consp item)
			  (member (first item)
				  '(SPEED SPACE COMPILATION-SPEED DEBUG SAFETY)))))))

;;; ----------------------------------------------------------------------
;;; GENERIC FUNCTION (RE)INITIALIZATION PROTOCOL
;;

(defun lambda-list-required-arguments (lambda-list)
  (rest (si::process-lambda-list lambda-list t)))

(defmethod shared-initialize :before
    ((gfun standard-generic-function) slot-names
     &key (name nil) (argument-precedence-order nil a-o-p)
       (lambda-list nil l-l-p) (declarations nil)
       (documentation nil) (method-class nil m-c-p)
     &allow-other-keys
     &aux (gfun-name (or (core:function-name gfun) name :anonymous)))
  (declare (ignore slot-names))
  ;; Check the validity of several fields.
  (when a-o-p
    (unless l-l-p
      (core:simple-program-error "When defining generic function ~A
Supplied :argument-precedence-order, but :lambda-list is missing"
			         gfun-name))
    (dolist (l (lambda-list-required-arguments lambda-list))
      (unless (= (count l argument-precedence-order) 1)
	(core:simple-program-error "When defining generic function ~A
The required argument ~A does not appear exactly once in the ARGUMENT-PRECEDENCE-ORDER list ~A"
			           gfun-name l argument-precedence-order))))
  (unless (every #'valid-declaration-p declarations)
    (core:simple-program-error "When defining generic function ~A
Not a valid declaration list: ~A"
                               gfun-name declarations))
  (unless (typep documentation '(or null string))
    (error 'simple-type-error
	   :format-control "When defining generic function~A
Not a valid documentation object ~A"
	   :format-arguments (list gfun-name documentation)
	   :datum documentation
	   :expected-type '(or null string)))
  (unless (or (not m-c-p) (si::subclassp method-class (find-class 'method)))
    (core:simple-program-error "When defining generic function~A~%Not a valid method class, ~A"
                               gfun-name method-class))
  ;; When supplying a new lambda-list, ensure that it is compatible with
  ;; the old list of methods.
  (when (and l-l-p (slot-boundp gfun 'methods))
    (unless (every #'(lambda (x)
                       (declare (core:lambda-name shared-initialize.lambda))
		       (congruent-lambda-p lambda-list x))
		 (mapcar #'method-lambda-list (generic-function-methods gfun)))
      (core:simple-program-error "Cannot replace the lambda list of ~A with ~A because it is incongruent with some of the methods"
			         gfun lambda-list))))

(defun congruent-lambda-p (l1 l2)
  (multiple-value-bind (r1 opts1 rest1 key-flag1 keywords1 a-o-k1)
      (core:process-lambda-list l1 'FUNCTION)
    (multiple-value-bind (r2 opts2 rest2 key-flag2 keywords2 a-o-k2)
	(core:process-lambda-list l2 'FUNCTION)
      (and (= (length r2) (length r1))
        (= (length opts1) (length opts2))
        (eq (and (null rest1) (null key-flag1))
            (and (null rest2) (null key-flag2)))
        ;; All keywords mentioned in the genericf function
        ;; must be accepted by the method.
        (or (null key-flag1)
          (null key-flag2)
          ;; Testing for a-o-k1 here may not be conformant when
          ;; the fourth point of 7.6.4 is read literally, but it
          ;; is more consistent with the generic function calling
          ;; specification. Also it is compatible with popular
          ;; implementations like SBCL and CCL. -- jd 2020-04-07
          a-o-k1
          a-o-k2
          (null (set-difference (all-keywords keywords1)
                                (all-keywords keywords2))))
        t))))

(defun all-keywords (l)
  (let ((all-keys '()))
    (do ((l (rest l) (cddddr l)))
	((null l)
	 all-keys)
      (push (first l) all-keys))))

(defun initialize-gf-specializer-profile (gfun)
  (when (slot-boundp gfun 'lambda-list)
    (let* ((lambda-list (generic-function-lambda-list gfun))
           (nreq (length (lambda-list-required-arguments lambda-list))))
      (setf (generic-function-specializer-profile gfun)
            (make-array nreq :initial-element nil)))))

(defmethod shared-initialize :after
    ((gfun standard-generic-function) slot-names
     &key (name nil name-p) (lambda-list nil l-l-p)
       (documentation nil documentation-p)
       (argument-precedence-order nil a-o-p)
       ;; Use a CLOS symbol in case someone else wants a :source-position initarg.
       ((source-position spi) nil spi-p)
       &allow-other-keys)
  (declare (ignore slot-names argument-precedence-order)
           (core:lambda-name shared-initialize.generic-function))
  ;; Coerce a method combination if required.
  (let ((combination (generic-function-method-combination gfun)))
    (unless (typep combination 'method-combination)
      (setf (%generic-function-method-combination gfun)
	    (find-method-combination gfun (first combination) (rest combination)))))
  ;; If we have a new lambda list but no argument precedence, default the latter.
  (when (and l-l-p (not a-o-p))
    (setf (%generic-function-argument-precedence-order gfun)
	  (lambda-list-required-arguments lambda-list)))
  ;; If we have a new name, set the internal name.
  ;; If there's no new name, but the old name isn't set, set it to the default LAMBDA.
  ;; NOTE: MOP says it should be NIL, but we use LAMBDA elsewhere. Could fix that.
  (if name-p
      (core:setf-function-name gfun name)
      (when (eq (core:function-name gfun) (core:unbound))
        (core:setf-function-name gfun 'cl:lambda)))
  ;; If we have a new lambda list, set the display lambda list.
  (when l-l-p
    (core:setf-lambda-list gfun lambda-list))
  ;; Ditto docstring.
  (when documentation-p
    (setf (core:function-docstring gfun) documentation))
  ;; If we have a source position, set that.
  (when spi-p
    ;; FIXME: Too many fields and the underlying function makes a new SPI. Dumb.
    (core:set-source-pos-info
     gfun
     (core:file-scope-pathname
      (core:file-scope
       (core:source-pos-info-file-handle spi)))
     (core:source-pos-info-filepos spi)
     (core:source-pos-info-lineno spi)
     ;; 1+ copied from cmpir.lisp. Dunno why it's there.
     (1+ (core:source-pos-info-column spi))))
  (cond ((generic-function-methods gfun)
         (compute-gf-specializer-profile gfun)
         (compute-a-p-o-function gfun))
        (t (initialize-gf-specializer-profile gfun))))

;;; Recompute the specializer profile entirely.
;;; Needed if a method has been removed.
(defun compute-gf-specializer-profile (gf)
  (setf (generic-function-specializer-profile gf)
        ;; NOTE: If the gf has no methods, this results in a
        ;; specializer profile of NIL, which is not a vector.
        ;; This can cause errors in code that expects the sp to be
        ;; a vector, but the sp being NIL in code like that indicates
        ;; some kind of bug. We could use #() here instead, but that
        ;; would just mask such bugs.
        (let ((sp nil))
          (dolist (method (generic-function-methods gf))
            (let ((specializers (method-specializers method)))
              (when (null sp)
                (setf sp (make-array (length specializers))))
              (update-specializer-profile sp specializers)))
          sp)))

(defun update-specializer-profile (specializer-profile specializers)
  (loop for spec in specializers
        for i from 0
        for e = (svref specializer-profile i)
        do (setf (svref specializer-profile i)
                 (cond ((typep spec 'eql-specializer)
                        (let ((o (eql-specializer-object spec)))
                          ;; Add to existing list of eql spec
                          ;; objects, or make a new one.
                          (if (consp e)
                              (adjoin o e)
                              (list o))))
                       ((eql spec #.(find-class 't)) (or e nil))
                       (t (or e t)))))
  specializer-profile)

(defun compute-a-p-o-function (gf)
  (let ((a-p-o (generic-function-argument-precedence-order gf))
        (gf-ll (generic-function-lambda-list gf)))
    (setf (generic-function-a-p-o-function gf)
          (if (consp gf-ll)
              (let ((required-arguments (rest (core:process-lambda-list gf-ll t))))
                (if (equal a-p-o required-arguments)
                    nil
                    (coerce `(lambda (%list)
                               (destructuring-bind ,required-arguments %list
                                 (list ,@a-p-o)))
                            'function)))
              nil))))

(defmethod initialize-instance :after ((gfun standard-generic-function) &rest initargs)
  (declare (ignore initargs))
  (invalidate-discriminating-function gfun))

(defmethod reinitialize-instance :after ((gfun standard-generic-function) &rest initargs)
  (update-dependents gfun initargs)
  ;; Check if the redefinition is trivial.
  ;; I am not sure of the fine details here. What happens if you reinitialize-instance
  ;; and change the method-combination, but not the methods to have compatible qualifiers,
  ;; for example? So what I'm going with is a somewhat magical minimum:
  ;; ENSURE-GENERIC-FUNCTION-USING-CLASS below calls with :name and whatever args it was
  ;; passed, and DEFMETHOD will pass with no extra args.
  ;; By incorporating this case, we avoid erasing the entire call history after any defmethod.
  ;; Note that ADD-METHOD is smart and does modify the call history to include the new method,
  ;; So things will remain consistent.
  (unless (and (= (length initargs) 2)
               (eq (first initargs) :name))
    ;; OK, something complicated. Erase.
    (erase-generic-function-call-history gfun)
    (invalidate-discriminating-function gfun)))

(defun erase-generic-function-call-history (gf)
  (setf (mp:atomic (generic-function-call-history gf)) nil))

(defun associate-methods-to-gfun (name &rest methods)
  (let ((gfun (fdefinition name)))
    (dolist (method methods)
      (setf (getf (method-plist method) :method-from-defgeneric-p) t))
    gfun))

(defmethod ensure-generic-function-using-class
    ((gfun generic-function) name
     &rest args
     &key
       (method-class 'STANDARD-METHOD method-class-p)
       (generic-function-class (class-of gfun) gfcp)
       (delete-methods nil)
       &allow-other-keys)
  ;; modify the existing object
  (setf args (copy-list args))
  (remf args :generic-function-class)
  (remf args :declare)
  (remf args :environment)
  (remf args :delete-methods)
  (when (symbolp generic-function-class)
    (setf generic-function-class (find-class generic-function-class)))
  (when gfcp
    ;; ANSI DEFGENERIC talks about the possibility of change-class-ing a
    ;; generic function, but AMOP specifically rules this possibility out.
    ;; We go with the latter.
    (unless (eq generic-function-class (class-of gfun))
      (error "Cannot change the class of generic function ~a from ~a to ~a. See AMOP, ENSURE-GENERIC-FUNCTION-USING-CLASS."
             name (class-name (class-of gfun)) (class-name generic-function-class))))
  (when (and method-class-p (symbolp method-class))
    (setf args (list* :method-class (find-class method-class) args)))
  (when delete-methods
    (dolist (m (copy-list (generic-function-methods gfun)))
      (when (getf (method-plist m) :method-from-defgeneric-p)
	(remove-method gfun m))))
  (if (eq (class-of gfun) generic-function-class)
      (apply #'reinitialize-instance gfun :name name args)
      (apply #'change-class gfun generic-function-class :name name args)))

(defmethod ensure-generic-function-using-class
    ((gfun null) name &rest args &key
                                   (method-class 'STANDARD-METHOD method-class-p)
                                   (generic-function-class 'STANDARD-GENERIC-FUNCTION)
                                   (delete-methods nil)
                                   &allow-other-keys)
  (declare (ignore delete-methods gfun))
  ;; else create a new generic function object
  (setf args (copy-list args))
  (remf args :generic-function-class)
  (remf args :declare)
  (remf args :environment)
  (remf args :delete-methods)
  (when (and method-class-p (symbolp method-class))
    (setf args (list* :method-class (find-class method-class) args)))
  (when (symbolp generic-function-class)
    (setf generic-function-class (find-class generic-function-class)))
  (unless (si::subclassp generic-function-class (find-class 'generic-function))
    (error "~A is not a valid :GENERIC-FUNCTION-CLASS argument for ENSURE-GENERIC-FUNCTION, as it is not a subclass of GENERIC-FUNCTION."
	   generic-function-class))
  (apply #'make-instance generic-function-class :name name args))

;;; Miscellany

;;; Returns as process-lambda-list, but with keys etc from the methods added to the gf's.
;;; Basically returns the valid lambda list as in CLHS 7.6.5.
(defun generic-function-augmented-lambda-list (gf applicable-methods)
  (multiple-value-bind (req opt restvar keyflag keysl aokp) ; aux, varest irrelevant
      (core:process-lambda-list (generic-function-lambda-list gf) 'function)
    ;; get a list of keywords accepted by the gf.
    (let ((known-keys (loop for (key) on (cdr keysl) by #'cddddr
                            collecting key)))
      (loop for method in applicable-methods
            do (multiple-value-bind (mreq mopt mrestvar mkeyflag mkeysl maokp)
                   (core:process-lambda-list (method-lambda-list method) 'function)
                 (declare (ignore mreq mopt mrestvar))
                 ;; If any method wants keywords the arguments have to match that.
                 ;; E.g. for when the gf has only &rest but a method has &key.
                 (unless keyflag (when mkeyflag (setf keyflag t)))
                 ;; If any method has &allow-other-keys, so does the effective method.
                 (unless aokp (when maokp (setf aokp t)))
                 ;; The acceptable keywords are the union of the gf's with the methods'.
                 ;; The variables, default values, and -p variables are irrelevant in
                 ;; this function.
                 ;; We collect the keywords even if aokp is true because we might also
                 ;; want to use this lambda list for a display to the programmer.
                 (loop for (key var default -p) on (cdr mkeysl) by #'cddddr
                       unless (member key known-keys)
                         ;; new keys - throw em in front
                         do (setf (cdr keysl) (list* key var default -p (cdr keysl))
                                  (car keysl) (1+ (car keysl))
                                  ;; also update the known-keys for later methods.
                                  known-keys (list* key known-keys))))))
    (values req opt restvar keyflag keysl aokp nil nil)))

(defmethod add-dependent ((c standard-generic-function) dependent)
  (pushnew dependent (generic-function-dependents c)))

(defmethod remove-dependent ((c standard-generic-function) dep)
  (setf (generic-function-dependents c)
        (remove dep (generic-function-dependents c))))

(defmethod map-dependents ((c standard-generic-function) function)
  (dolist (d (generic-function-dependents c))
    (funcall function d)))

(defmethod (setf generic-function-name) (name (gf standard-generic-function))
  (reinitialize-instance gf :name name)
  name)

(defun compile-generic-function-methods (gf)
  (loop with overall-warningsp = nil
        with overall-failurep = nil
        for method in (generic-function-methods gf)
        do (multiple-value-bind (_ warningsp failurep)
               (compile-method method)
             (declare (ignore _))
             (setf overall-warningsp (or overall-warningsp warningsp)
                   overall-failurep (or overall-failurep failurep)))
        finally (return
                  (values gf overall-warningsp overall-failurep))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Adding, removing, and finding methods
;;; This code is here rather than method.lisp because the operations on the
;;; generic are more involved than those on the methods.
;;;

(defmethod add-method ((gf standard-generic-function) (method standard-method))
  ;;
  ;; 1) The method must not be already installed in another generic function.
  ;;
  (let ((other-gf (method-generic-function method)))
    (unless (or (null other-gf) (eq other-gf gf))
      (error "The method ~A belongs to the generic function ~A ~
and cannot be added to ~A." method other-gf gf)))
  ;;
  ;; 2) The method and the generic function should have congruent lambda
  ;;    lists. That is, it should accept the same number of required and
  ;;    optional arguments, and only accept keyword arguments when the generic
  ;;    function does.
  ;;
  (let ((new-lambda-list (method-lambda-list method)))
    (if (slot-boundp gf 'lambda-list)
	(let ((old-lambda-list (generic-function-lambda-list gf)))
	  (unless (congruent-lambda-p old-lambda-list new-lambda-list)
	    (error "Cannot add the method ~A to the generic function ~A because their lambda lists ~A and ~A are not congruent."
		   method gf new-lambda-list old-lambda-list))
          ;; Add any keywords from the method to the gf display lambda list.
          (maybe-augment-generic-function-lambda-list gf new-lambda-list))
        (reinitialize-instance
         gf :lambda-list (method-lambda-list-for-gf new-lambda-list))))
  ;;
  ;; 3) Finally, it is inserted in the list of methods, and the method is
  ;;    marked as belonging to a generic function.
  ;;
  (when (generic-function-methods gf)
    (let* ((method-qualifiers (method-qualifiers method)) 
	   (specializers (method-specializers method))
	   (found (find-method gf method-qualifiers specializers nil)))
      (when found
	(remove-method gf found))))
  ;;
  ;; Per AMOP's description of ADD-METHOD, we install the method by:
  ;;  i) Adding it to the list of methods.
  (push method (%generic-function-methods gf))
  (setf (%method-generic-function method) gf)
  ;;  ii) Adding the method to each specializer's direct-methods.
  (register-method-with-specializers method)
  ;;  iii) Computing a new discriminating function.
  ;;       Though in this case it will be the invalidated function.
  (update-gf-specializer-profile gf (method-specializers method))
  (compute-a-p-o-function gf)
  (update-generic-function-call-history-for-add-method gf method)
  (set-funcallable-instance-function gf (compute-discriminating-function gf))
  ;;  iv) Updating dependents.
  (update-dependents gf (list 'add-method method))
  gf)

;;; auxiliary for add-method
;;; It takes a DEFMETHOD lambda list and returns a lambda list usable for
;;; initializing a generic function. The difficulty here is that the CLHS
;;; page for DEFMETHOD specifies that if a generic function is implicitly
;;; created, its lambda list lacks any specific keyword parameters.
;;; So (defmethod foo (... &key a)) (defmethod foo (... &key)) is legal.
;;; If we were to just use the same method lambda list, this would not be
;;; true.
(defun method-lambda-list-for-gf (lambda-list)
  (multiple-value-bind (req opt rest keyflag keywords aok)
      (core:process-lambda-list lambda-list 'function)
    (declare (ignore keywords))
    `(,@(rest req)
      ,@(unless (zerop (car opt))
          (cons '&optional (loop for (o) on (rest opt)
                                 by #'cdddr
                                 collect o)))
      ,@(when rest (list '&rest rest))
      ,@(when keyflag '(&key))
      ,@(when aok '(&allow-other-keys)))))

(defun register-method-with-specializers (method)
  (loop for spec in (method-specializers method)
        do (add-direct-method spec method)))

(defun update-gf-specializer-profile (gf specializers)
  ;; Although update-specializer-profile mutates the vector,
  ;; we still need this setf for the case in which the existing sp
  ;; was NIL (see compute-gf-specializer-profile below for how this
  ;; can arise).
  (setf (generic-function-specializer-profile gf)
        (let* ((sv (generic-function-specializer-profile gf))
               (to-update (or sv (make-array (length specializers)
                                             :initial-element nil))))
          (update-specializer-profile to-update specializers))))

;;; This "fuzzed" applicable-method-p is used in
;;; update-call-history-for-add-method, below, to handle added EQL-specialized
;;; methods properly. See bug #1009.
(defun fuzzed-applicable-method-p (method specializers)
  (loop for spec in (method-specializers method)
        for argspec in specializers
        always (cond ((typep spec 'eql-specializer)
                      (if (eql-specializer-p argspec)
                          (eql (eql-specializer-object argspec)
                               (eql-specializer-object spec))
                          (si:subclassp argspec
                                     (class-of (eql-specializer-object spec)))))
                     ((eql-specializer-p argspec)
                      (si:subclassp (class-of (eql-specializer-object argspec))
                                 spec))
                     (t (si:subclassp argspec spec)))))

(defun update-call-history-for-add-method (call-history method)
  "When a method is added then we update the effective-method-functions for
   those call-history entries with specializers that the method would apply to."
  (loop for entry in call-history
        for specializers = (coerce (car entry) 'list)
        unless (fuzzed-applicable-method-p method specializers)
          collect entry))

(defun update-generic-function-call-history-for-add-method (generic-function method)
  "When a method is added then we update the effective-method-functions for
   those call-history entries with specializers that the method would apply to.
FIXME!!!! This code will have problems with multithreading if a generic function is in flight. "
  (mp:atomic-update (generic-function-call-history generic-function)
                    #'update-call-history-for-add-method method))

(defmethod remove-method ((gf standard-generic-function) (method standard-method))
  (setf (%generic-function-methods gf)
	(delete method (generic-function-methods gf))
	(%method-generic-function method) nil)
  (loop for spec in (method-specializers method)
     do (remove-direct-method spec method))
  (compute-gf-specializer-profile gf)
  (compute-a-p-o-function gf)
  (update-generic-function-call-history-for-remove-method gf method)
  (set-funcallable-instance-function gf (compute-discriminating-function gf))
  (update-dependents gf (list 'remove-method method))
  gf)

(defun update-call-history-for-remove-method (call-history method)
  (let (new-call-history)
    (loop for entry in call-history
          for specializers = (coerce (car entry) 'list)
          unless (method-applicable-to-specializers-p method specializers)
            do (push (cons (car entry) (cdr entry)) new-call-history))
    new-call-history))

(defun update-generic-function-call-history-for-remove-method (generic-function method)
  "When a method is removed then we update the effective-method-functions for
   those call-history entries with specializers that the method would apply to
    AND if that means there are no methods left that apply to the specializers
     then remove the entry from the list.
FIXME!!!! This code will have problems with multithreading if a generic function is in flight. "
  (mp:atomic-update (generic-function-call-history generic-function)
                    #'update-call-history-for-remove-method method))

(defmethod find-method ((gf standard-generic-function) qualifiers specializers
                        &optional (errorp t))
  (flet ((filter-specializer (name)
	   (cond ((typep name 'specializer)
		  name)
		 ((atom name)
		  (let ((class (find-class name nil)))
		    (unless class
		      (error "~A is not a valid specializer name" name))
		    class))
		 ((and (eq (first name) 'EQL)
		       (null (cddr name)))
                  (intern-eql-specializer (second name)))
		 (t
		  (error "~A is not a valid specializer name" name))))
	 (specializer= (cons-or-class specializer)
           (eq cons-or-class specializer)))
    (when (/= (length specializers)
	      (length (generic-function-argument-precedence-order gf)))
      (error
       "The specializers list~%~A~%does not match the number of required arguments (~a) in ~A"
       specializers
       (length (generic-function-argument-precedence-order gf))
       (generic-function-name gf)))
    (loop with specializers = (mapcar #'filter-specializer specializers)
       for method in (generic-function-methods gf)
       when (and (equal qualifiers (method-qualifiers method))
		 (every #'specializer= specializers (method-specializers method)))
       do (return-from find-method method))
    ;; If we did not find any matching method, then the list of
    ;; specializers might have the wrong size and we must signal
    ;; an error.
    (when errorp
      (error "There is no method on the generic function ~S that agrees on qualifiers ~S and specializers ~S"
	     (generic-function-name gf)
	     qualifiers specializers)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENSURE-GENERIC-FUNCTION

(defun legal-generic-function-name-p (name)
  (si::valid-function-name-p name))

(defun ensure-generic-function (name &rest args &key &allow-other-keys)
  (let ((gfun (si::traced-old-definition name)))
    (cond ((not (legal-generic-function-name-p name))
	   (core:simple-program-error "~A is not a valid generic function name" name))
          ((not (fboundp name))
	   (setf (fdefinition name)
		 (apply #'ensure-generic-function-using-class gfun name args)))
          ((si::instancep (or gfun (setf gfun (fdefinition name))))
	   (let ((new-gf (apply #'ensure-generic-function-using-class gfun name args)))
	     new-gf))
	  ((special-operator-p name)
	   (core:simple-program-error "The special operator ~A is not a valid name for a generic function" name))
	  ((macro-function name)
	   (core:simple-program-error
            "The symbol ~A is bound to a macro and is not a valid name for a generic function" name))
	  (t
	   (core:simple-program-error "The symbol ~A is bound to an ordinary function and is not a valid name for a generic function" name)))))
