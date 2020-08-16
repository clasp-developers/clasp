#+(or)(eval-when (:execute)
        (format t "!~%!~%!~%!~%!~%In generic.lsp !~%  Turning on :compare *feature*  for ensure-generic-function~%!~%!~%!~%!~%")
        (setq cl:*features* (cons :compare cl:*features*))
        (setq cl:*features* (cons :force-lots-of-gcs cl:*features*)))
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

;;; ----------------------------------------------------------------------
;;; DEFGENERIC
;;;

(defmacro defgeneric (&whole whole &rest args)
  (multiple-value-bind (function-specifier lambda-list options)
      (parse-defgeneric args)
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
	    output)))))

(defun parse-defgeneric (args)
  ;; (values function-specifier lambda-list options)
  (let (function-specifier)
    (unless args
      (simple-program-error "Illegal defgeneric form: missing generic function name"))
    (setq function-specifier (pop args))
    (unless args
      (simple-program-error "Illegal defgeneric form: missing lambda-list"))
    (values function-specifier (first args) (rest args))))

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
                 (simple-program-error "Option ~s specified more than once"
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
                            (simple-program-error "Too many arguments for option ~A"
                                                  option-name))
                          (second option))
                         (otherwise
                          (simple-program-error "~S is not a legal defgeneric option"
                                                option-name))))
                 (setf arg-list `(',option-name ',option-value ,@arg-list)))))))
    (values `(:lambda-list ',lambda-list ,@arg-list
              ,@(when core:*current-source-pos-info*
                  (list ''source-position core:*current-source-pos-info*))
	      ,@(when declarations `(:declarations ',declarations)))
	    method-list)))

;;; For effect. Checks whether the lambda list is well formed.
(defun parse-lambda-list (lambda-list &optional post-keyword)
  (ext:with-current-source-form (lambda-list)
    (let ((arg (car lambda-list)))
      (cond ((null lambda-list))
            ((eq arg '&AUX)
             (simple-program-error "&aux is not allowed in a generic function lambda-list"))
            ((member arg lambda-list-keywords)
             (parse-lambda-list (cdr lambda-list) t))
            (post-keyword
             ;; After a lambda-list-keyword there can be no specializers.
             (parse-lambda-list (cdr lambda-list) t))
            (t
             (if (listp arg)
                 (simple-program-error "the parameters cannot be specialized in generic function lambda-list")
                 (parse-lambda-list (cdr lambda-list))))))))

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
    ((gfun standard-generic-function) slot-names &rest initargs
     &key (name nil) (argument-precedence-order nil a-o-p)
       (lambda-list nil l-l-p) (declarations nil)
       (documentation nil) (method-class nil m-c-p)
     &allow-other-keys
     &aux (gfun-name (or (core:function-name gfun) name :anonymous)))
  ;; Check the validity of several fields.
  (when a-o-p
    (unless l-l-p
      (simple-program-error "When defining generic function ~A
Supplied :argument-precedence-order, but :lambda-list is missing"
			    gfun-name))
    (dolist (l (lambda-list-required-arguments lambda-list))
      (unless (= (count l argument-precedence-order) 1)
	(simple-program-error "When defining generic function ~A
The required argument ~A does not appear exactly once in the ARGUMENT-PRECEDENCE-ORDER list ~A"
			      gfun-name l argument-precedence-order))))
  (unless (every #'valid-declaration-p declarations)
    (simple-program-error "When defining generic function ~A
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
    (simple-program-error "When defining generic function~A~%Not a valid method class, ~A"
                          gfun-name method-class))
  ;; When supplying a new lambda-list, ensure that it is compatible with
  ;; the old list of methods.
  (when (and l-l-p (slot-boundp gfun 'methods))
    (unless (every #'(lambda (x)
                       (declare (core:lambda-name shared-initialize.lambda))
		       (congruent-lambda-p lambda-list x))
		 (mapcar #'method-lambda-list (generic-function-methods gfun)))
      (simple-program-error "Cannot replace the lambda list of ~A with ~A because it is incongruent with some of the methods"
			    gfun lambda-list))))

(defun initialize-gf-specializer-profile (gfun)
  (when (slot-boundp gfun 'lambda-list)
    (let* ((lambda-list (generic-function-lambda-list gfun))
           (nreq (length (lambda-list-required-arguments lambda-list))))
      (setf (generic-function-specializer-profile gfun)
            (make-array nreq :initial-element nil)))))

(defmethod shared-initialize :after
    ((gfun standard-generic-function) slot-names &rest initargs
     &key (name nil name-p) (lambda-list nil l-l-p)
       (documentation nil documentation-p)
       (argument-precedence-order nil a-o-p)
       ;; Use a CLOS symbol in case someone else wants a :source-position initarg.
       ((source-position spi) nil spi-p)
       &allow-other-keys)
  (declare (ignore slot-names)
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
      (setf-function-name gfun name)
      (when (eq (core:function-name gfun) (core:unbound))
        (setf-function-name gfun 'cl:lambda)))
  ;; If we have a new lambda list, set the display lambda list.
  (when l-l-p
    (setf-lambda-list gfun lambda-list))
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
     ;; 1+ copied from cmpir.lsp. Dunno why it's there.
     (1+ (core:source-pos-info-column spi))))
  (cond ((generic-function-methods gfun)
         (compute-gf-specializer-profile gfun)
         (compute-a-p-o-function gfun))
        (t (initialize-gf-specializer-profile gfun))))

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
  (mlog "In ensure-generic-function-using-class (gfun generic-function) gfun -> %s  name -> %s args -> %s%N" gfun name args)
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
      (progn
	(apply #'reinitialize-instance gfun :name name args))
      (progn
	(apply #'change-class gfun generic-function-class :name name args))))

(defmethod ensure-generic-function-using-class
    ((gfun null) name &rest args &key
                                   (method-class 'STANDARD-METHOD method-class-p)
                                   (generic-function-class 'STANDARD-GENERIC-FUNCTION)
                                   (delete-methods nil)
                                   &allow-other-keys)
  (declare (ignore delete-methods gfun))
  (mlog "In ensure-generic-function-using-class (gfun generic-function) gfun -> %s  name -> %s args -> %s%N" gfun name args)
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

;;; Kind of badly placed, but- returns minimum and maximum number of args allowed as values.
;;; max is NIL if infinite. Used by fastgf.
(defun generic-function-min-max-args (gf)
  ;; since we call this from fastgf, it can't use generic functions (like g-f-l-l)
  ;; but FIXME: this may be a problem if g-f-l-l being generic is relevant, e.g. for a user subclass.
  (with-early-accessors (+standard-generic-function-slots+)
    (multiple-value-bind (req opt restvar keyflag) ; rest are irrelevant
        (core:process-lambda-list (generic-function-lambda-list gf) 'function)
      (values (car req) (if (or restvar keyflag) nil (+ (car req) (car opt)))))))
