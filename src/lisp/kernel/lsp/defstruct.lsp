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
;;;;        The structure routines.

(in-package "SYSTEM")

#+(or)(eval-when (:load-toplevel :compile-toplevel :execute)
  (setq *echo-repl-read* t)
)
(defun structure-type-error (value slot-type struct-name slot-name)
  (error 'simple-type-error
	 :format-control "Slot ~A in structure ~A only admits values of type ~A."
	 :format-arguments (list slot-name struct-name slot-type)
	 :datum value
	 :expected-type slot-type))

;;; FIXME: these should take environments
(defun structure-type (name)
  (get-sysprop name 'structure-type))
(defun (setf structure-type) (type name)
  (put-sysprop name 'structure-type type))
(defun structure-size (name)
  (get-sysprop name 'structure-offset))
(defun (setf structure-size) (size name)
  (put-sysprop name 'structure-offset size))
(defun structure-slot-descriptions (name)
  (get-sysprop name 'structure-slot-descriptions))
(defun (setf structure-slot-descriptions) (descriptions name)
  (put-sysprop name 'structure-slot-descriptions descriptions))
(defun structure-constructor (name)
  (get-sysprop name 'structure-constructor))
(defun (setf structure-constructor) (constructor name)
  (put-sysprop name 'structure-constructor constructor))
(defun names-structure-p (name)
  (or (structure-type name)
      (let ((class (find-class name nil)))
        (and class (typep class 'structure-class)))))

(defun make-access-function (name conc-name type named slot-descr)
  (declare (ignore named))
  (let* ((slot-name (nth 0 slot-descr))
	 ;; (default-init (nth 1 slot-descr))
	 ;; (slot-type (nth 2 slot-descr))
	 (read-only (nth 3 slot-descr))
	 (offset (nth 4 slot-descr))
	 (access-function (if conc-name
			      (intern (base-string-concatenate conc-name slot-name))
			      slot-name)))
    (if (eql access-function (sixth slot-descr))
	(return-from make-access-function nil)
	(setf (sixth slot-descr) access-function))
    (cond ((null type)
           ;; If TYPE is NIL,
           ;;  the slot is at the offset in the structure-body.
	   (fset access-function
                 #'(lambda (x)
                     ;; FIXME: load-time-value or something
                     (unless (core:subclassp (class-of x) (find-class name))
                       (signal-type-error x name))
                     (si:instance-ref x offset))))
          ((subtypep type '(OR LIST VECTOR))
	   ;; If TYPE is VECTOR, (VECTOR ... ) or LIST, ELT is used.
           (fset access-function
		 #'(lambda (x) (elt x offset))))
          (t (error "~S is an illegal structure type." type)))
    (cond (read-only
	   (fmakunbound `(setf ,access-function))
	   (set-documentation access-function 'SETF nil))
	  ;; The following is used by the compiler to expand inline
	  ;; the accessor
	  (t
	   (do-setf-structure-method access-function (or type name)
				     offset)))))

(defun do-setf-structure-method (access-function type index)
  (declare (optimize (speed 3) (safety 0)))
  (do-defsetf access-function
    (cond ((or (eq type 'list) (eq type 'vector))
	   #'(lambda (newvalue struct)
	       `(sys:setf-elt ,struct ,index ,newvalue)))
	  ((consp type)
	   #'(lambda (newvalue struct)
	       `(si:row-major-aset (the ,type ,struct) ,index ,newvalue)))
	  (t
	   #'(lambda (newvalue struct)
               `(progn
                  (unless (core:subclassp (class-of ,struct) (find-class ',type))
                    (signal-type-error ,struct ',type))
                  (si:instance-set ,struct ,index ,newvalue)))))))

(defun process-boa-lambda-list (slot-names slot-descriptions boa-list assertions)
  (let ((mentioned-slots '())
	(aux))
    ;; With a call to PROCESS-LAMBDA-LIST we ensure that the lambda list is
    ;; syntactically correct. This simplifies notably the code in the loop.
    ;; meister: This just runs through the lambda list and generates a LambdaListHandler
    ;;          that is discarded.  If there are problems with the lambda-list then
    ;;          an error is thrown
    (make-lambda-list-handler (setq boa-list (copy-list boa-list)) nil 'function)
    ;; Search for &optional or &key arguments without initialization.  Also,
    ;; record all slot names which are initialized by means of the BOA call.
    (do* ((i boa-list (rest i))
	  (slot (first i) (first i))
	  (modify nil))
	 ((endp i))
      (cond ((or (eq slot '&optional) (eq slot '&key))
	     (setq modify t))
	    ((eq slot '&rest)
	     (setq modify nil))
	    ((eq slot '&aux)
	     (setq aux t modify nil))
	    ((eq slot '&allow-other-keys)
	     )
	    ((atom slot)
	     (push slot mentioned-slots)
	     (when modify
	       (setf (first i)
		     (list slot (second (assoc slot slot-descriptions)))))
	     (when aux
	       (setf assertions (delete slot assertions :key 'cadadr))))
	    (t
	     (let ((slot-name (first slot)))
	       (when (consp slot-name)
		 (setq slot-name (second slot-name)))
	       (push slot-name mentioned-slots)
	       (when (endp (rest slot))
		 (when modify
		   (setf (rest slot)
			 (list (second (assoc slot-name slot-descriptions)))))
		 (when aux
		   (setf assertions (delete slot assertions :key 'cadadr))))))))
    ;; For all slots not mentioned above, add the default values from
    ;; the DEFSTRUCT slot description.
    (let ((other-slots (nset-difference
			(delete-if #'consp (copy-list slot-names))
			mentioned-slots)))
      (do ((l other-slots (cdr l)))
	  ((endp l))
	(let* ((slot (assoc (car l) slot-descriptions))
	       (slot-init (second slot)))
	  (when slot-init
	    (setf (car l) (list (car l) slot-init)))))
      (when other-slots
	(unless aux
	  (push '&aux other-slots))
	(setf boa-list (nconc boa-list other-slots)))
      (values boa-list assertions))))

(defun make-constructor (name constructor type named slot-descriptions)
  (declare (ignore named))
  ;; CONSTRUCTOR := constructor-name | (constructor-name boa-lambda-list)
  (let* ((boa-constructor-p (consp constructor))
	 (keys (unless boa-constructor-p (list '&key)))
	 (constructor-name (if boa-constructor-p (first constructor) constructor))
	 (slot-names '())
	 (assertions '()))
    (dolist (slot slot-descriptions
	     (setq slot-names (nreverse slot-names) keys (nreverse keys)))
      (push
       (cond ((null slot)
	      ;; If slot-description is NIL, it is padding for initial-offset.
	      nil)
	     ((eql (first slot) 'TYPED-STRUCTURE-NAME)
	      ;; This slot is the name of a typed structure with name.
	      (list 'QUOTE (second slot)))
	     (t
	      (let* ((slot-name (first slot))
		     (slot-type (third slot))
		     (offset (fifth slot))
		     (init-form (second slot))
		     (var-name slot-name))
		;; Unless BOA constructors are used, we should avoid using
		;; slot names as lambda variables in the constructor.
		(unless boa-constructor-p
		  (setq var-name (copy-symbol slot-name))
		  (push (if init-form (list var-name init-form) var-name)
			keys))
		;; We insert type checks for every slot and only in the
		;; case of BOA lists we remove some of these checks for
		;; uninitialized slots.
		(unless (eq 'T slot-type)
		  (push `(unless (typep ,var-name ',slot-type)
			   (structure-type-error ,var-name ',slot-type ',name ',slot-name))
			assertions))
		var-name)))
       slot-names))
    (when boa-constructor-p
      (setf (values keys assertions)
	    (process-boa-lambda-list slot-names slot-descriptions
				     (second constructor) assertions)))
    (cond ((null type)
           `(defun ,constructor-name ,keys
	      ,@assertions
	      #-CLOS
              (sys:make-structure ',name ,@slot-names)
	      ;; the class is defined by an enclosing LET form
	      #+clos
              (sys:make-structure
               (let ((x (load-time-value (list nil))))
                 (or (car x) (car (rplaca x (find-class ',name))))) ,@slot-names)
              ))
	  ((subtypep type '(VECTOR T))
	   `(defun ,constructor-name ,keys
	     (vector ,@slot-names)))
          ((subtypep type 'VECTOR)
           `(defun ,constructor-name ,keys
              (make-array ',(list (length slot-names))
			  :element-type ',(closest-sequence-type type)
	       		  :initial-contents (list ,@slot-names))))
          ((eq type 'LIST)
           `(defun ,constructor-name ,keys
              (list ,@slot-names)))
          (t (error "~S is an illegal structure type" type)))))


(defun make-predicate (predicate name type named name-offset)
  (cond ((null type)
         `(defun ,predicate (object)
            ;; fixme: find-class ahead of time
            (si:subclassp (class-of object) (find-class ',name))))
        ((or (eq type 'vector)
             (and (consp type) (eq (car type) 'vector)))
         ;; The name is at the NAME-OFFSET in the vector.
         (unless named (error "The structure should be named."))
         `(defun ,predicate (object)
            (and (vectorp object)
                 (> (length object) ,name-offset)
                 (eq (row-major-aref object ,name-offset) ',name))))
        ((eq type 'LIST)
         ;; The name is at the NAME-OFFSET in the list.
         (unless named (error "The structure should be named."))
         `(defun ,predicate (object)
            (eq (nth ,name-offset object) ',name)))
        (t (error "~S is an illegal structure type." type))))

(defun make-copier (copier name type)
  (declare (ignore name))
  (cond ((null type)
         ;; will need the name here later, i think.
         `(defun ,copier (object) (copy-structure object)))
        ((or (eq type 'vector)
             (and (consp type) (eq (car type) 'vector)))
         `(defun ,copier (object) (copy-seq object)))
        ((eq type 'list)
         `(defun ,copier (object) (copy-list object)))
        (t (error "~s is an illegal structure type." type))))

;;; PARSE-SLOT-DESCRIPTION parses a user slot-description
;;;  and returns a list of the form:
;;;        (slot-name initform slot-type read-only)
;;;  the read-only parameter is a default. It can be nil, true, or :unspecified
;;;   (used for :include overrides)

(defun parse-slot-description (slot-description &optional read-only)
  (let* ((slot-type 'T) slot-name initform)
    (cond ((atom slot-description)
           (setq slot-name slot-description))
          ((endp (cdr slot-description))
           (setq slot-name (car slot-description)))
          (t
           (setq slot-name (car slot-description))
           (setq initform (cadr slot-description))
           (do ((os (cddr slot-description) (cddr os)) (o) (v))
               ((endp os))
             (setq o (car os))
             (when (endp (cdr os))
                   (error "~S is an illegal structure slot option."
                          os))
             (setq v (cadr os))
             (case o
               (:TYPE (setq slot-type v))
               (:READ-ONLY (setq read-only v))
               (t
                (error "~S is an illegal structure slot option."
                         os))))))
    (list slot-name initform slot-type read-only)))

;;; UNPARSE-SLOT-DESCRIPTION does the opposite, turning one of the above into
;;;  something that would work in DEFSTRUCT.
;;; This is for documentation purposes only (describe uses it) at the moment,
;;;  and it should probably remain this way.
;;; Note that we have no way of distinguishing "initform NIL" and "no initform",
;;;  (though Clasp does currently treat these the same)
;;;  one of several reasons this is not exact.

(defun unparse-slot-description (list)
  (let ((name (first list)) (initform (second list))
        (type (third list)) (read-only (fourth list)))
    `(,name ,initform
            ,@(when read-only `(:read-only ,read-only))
            ,@(unless (eq type t) `(:type ,type)))))

;;; OVERWRITE-SLOT-DESCRIPTIONS overwrites the old slot-descriptions
;;;  with the new descriptions which are specified in the
;;;  :include defstruct option.

(defun overwrite-slot-descriptions (new-slots old-slots)
  (do* ((output '())
        (old-slots old-slots (rest old-slots)))
       ((null old-slots)
        (nreverse output))
    (let* ((old-slot (first old-slots))
           (slot-name (first old-slot))
           (new-slot (first (member slot-name new-slots :key #'car))))
      (if (null new-slot)
          (setf new-slot old-slot)
          (let* ((old-read-only (fourth old-slot))
                 (new-read-only (fourth new-slot)))
            (cond ((and (null new-read-only)
                        old-read-only)
                   (error "Tried to turn a read only slot ~A into writtable."
                          slot-name))
                  ((eq new-read-only :unknown)
                   (setf new-read-only old-read-only)))
            (setf new-slot (copy-list new-slot)
                  (fourth new-slot) new-read-only
                  (fifth new-slot) (fifth old-slot) ; preserve offset
                  (sixth new-slot) (sixth old-slot))))
      (push new-slot output))))

               `(define-class-struct ,name ,conc-name ,include ,slot-descriptions
                  ,overwriting-slot-descriptions ,print-function ,print-object
                  ,constructors ,predicate ,copier ,documentation)

(defun defstruct-sd->defclass-sd (sd)
  (destructuring-bind (name initform type read-only) sd
    (declare (ignore read-only)) ; handled elsewhere.
    `(,name :initform ,initform :type ,type
            :initarg ,(intern (symbol-name name) "KEYWORD"))))

(defmacro define-class-struct-constructors (name constructors slot-descriptions
                                            overwriting-slot-descriptions
                                            included-slot-descriptions)
  ...)

(defmacro define-class-struct-accessors (name conc-name slot-descriptions)
  (let ((index 0))
    (labels ((accname (slot-name)
               (if conc-name
                   (base-string-concatenate conc-name slot-name)
                   slot-name))
             (one (sd)
               (destructuring-bind (slot-name initform type read-only) sd
                 (declare (ignore initform))
                 (prog1
                     (let* ((accname (accname slot-name))
                            (writer
                              (if read-only
                                  nil
                                  ;; FIXME: inlinable setf function would be nicer,
                                  ;; but i'm pretty sure we can't inline setf functions atm.
                                  `((defsetf ,accname (object) (new)
                                      (list 'si:instance-set object ,index new))))))
                       (list* `(declaim (ftype (function (,name) ,type) ,reader)
                                        (inline ,reader))
                              `(defun ,reader (object)
                                 (si:instance-ref object ,index))
                              writer))
                   (incf index)))))
      (mapcan #'one slot-descriptions))))

(defmacro define-class-struct (name conc-name include slot-descriptions
                               overwriting-slot-descriptions print-function
                               print-object constructors predicate
                               copier documentation
                               &environment env)
  `(progn
     (defclass ,name ,(and include (list include))
       (,@(mapcar #'defstruct-sd->defclass-sd overwriting-slot-descriptions)
        ,@(mapcar #'defstruct-sd->defclass-sd slot-descriptions))
       ,@(when documentation
           `((:documentation ,documentation)))
       (:metaclass structure-class))
     ,@(when print-function
         ;; print-function and print-object can be lambda exprs,
         ;; so we have to be safe about names.
         (let ((obj (gensym "OBJ")) (stream (gensym "STREAM")))
           `((defmethod print-object ((,obj ,name) ,stream)
               (,print-function ,obj ,stream 0)))))
     ,@(when print-object
         (let ((obj (gensym "OBJ")) (stream (gensym "STREAM")))
           `((defmethod print-object ((,obj ,name) ,stream)
               ,(print-object obj stream)))))
     ,@(when predicate
         ;; generic functions are now fast enough that this is
         ;; faster code than calling SUBCLASSP or whatnot.
         `((defgeneric ,predicate (object)
             (:method (object) nil)
             (:method ((object ,name)) t))))
     ,@(when copier
         ;; It might seem like we can do better here- basically copy
         ;; a fixed number of slots - but CLHS is clear that the copier
         ;; must be COPY-STRUCTURE, and so it has to deal correctly with subclasses.
         `((declaim (ftype (function (,name) ,name) ,copier) ; useless atm
                    (inline ,copier))
           (defun ,copier (object) (copy-structure object))))

     ;; Tricky bit: a class we're including might not be defined at compile time.
     ;; This is unlikely but means we have to do stupid things.
     ,@(let ((include-class (when include (find-class include nil env))))
         (cond ((and include (null include-class))
                ;; FIXME: should be a style warning at most
                (warn "Structure definition for ~a INCLUDEs ~a, unknown at compile time."
                      name include)
                `((unless (find-class include nil)
                    (error "Cannot define structure ~a - it INCLUDEs ~a, which is undefined."
                           ',name ',include))
                  (let ((all-slots (append (overwrite-slot-descriptions
                                            ',overwriting-slot-descriptions
                                            (structure-slot-descriptions ',include)))))
                    (eval (list 'define-class-struct-constructors ',name ',constructors
                                ',all-slots))
                    (eval (list 'define-class-struct-accessors ',name ',conc-name
                                ',all-slots)))))
               (t
                (let ((all-slots (append (overwrite-slot-descriptions
                                          overwriting-slot-descriptions
                                          (structure-slot-descriptions include)))))
                  `((define-class-struct-constructors ',name ',constructors ',all-slots)
                    (define-class-struct-accessors ',name ',conc-name ',all-slots))))))))

(defmacro define-structure-class (name include slot-descriptions
                                  print-function print-object)
  `(progn
     (defclass ,name ,(and include (list include))
       ,(mapcar
         #'(lambda (sd)
             (if sd
                 (list* (first sd)
                        :initform (second sd)
                        :initarg 
                        (intern (symbol-name (first sd))
                                (find-package 'KEYWORD))
                        (when (third sd) (list :type (third sd))))
                 nil))            ; for initial offset slots
         slot-descriptions)
       (:metaclass structure-class))
     
     ,@(when print-function
         `((defmethod print-object ((obj ,name) stream)
             (,print-function obj stream 0)
             obj)))
     ,@(when print-object
         `((defmethod print-object ((obj ,name) stream)
             (,print-object obj stream)
             obj)))))

;;; The DEFSTRUCT macro.

(defmacro defstruct (name&opts &rest slots &environment env)
  "Syntax: (defstruct
         {name | (name {:conc-name | (:conc-name prefix-string) |
                        :constructor | (:constructor symbol [lambda-list]) |
                        :copier | (:copier symbol) |
                        :predicate | (:predicate symbol) |
                        (:include symbol) |
                        (:print-function function) |
                        (:print-object function) |
                        (:type {vector | (vector type) | list}) |
                        :named |
                        (:initial-offset number)}*)}
         [doc]
         {slot-name |
          (slot-name [default-value-form] {:type type | :read-only flag}*) }*
         )
Defines a structure named by NAME.  The doc-string DOC, if supplied, is saved
as a STRUCTURE doc and can be retrieved by (documentation 'NAME 'structure)."
  (let* ((slot-descriptions slots) overwriting-slot-descriptions
         (name (if (consp name&opts) (first name&opts) name&opts))
         (options (when (consp name&opts) (rest name&opts)))
         (conc-name (base-string-concatenate name "-"))
         (default-constructor (intern (base-string-concatenate "MAKE-" name)))
         (copier (intern (base-string-concatenate "COPY-" name)))
         (predicate (intern (base-string-concatenate name "-P")))
         constructors no-constructor standard-constructor
         predicate-specified include
         print-function print-object type named initial-offset
         (size 0) name-offset documentation)

    ;; Parse the defstruct options.
    (do ((os options (cdr os)) (o) (v))
        ((endp os))
      (cond ((and (consp (car os)) (not (endp (cdar os))))
             (setq o (caar os) v (cadar os))
             (case o
               (:CONC-NAME
                (if (null v)
                    (setq conc-name nil)
                    (setq conc-name v)))
               (:CONSTRUCTOR
                (if (null v)
                    (setq no-constructor t)
                    (if (endp (cddar os))
                        (setq constructors (cons v constructors))
                        (setq constructors (cons (cdar os) constructors)))))
               (:COPIER (setq copier v))
               (:PREDICATE
                (setq predicate v)
                (setq predicate-specified t))
               (:INCLUDE
                (setq include (cdar os))
                (unless (names-structure-p v)
                        (error "~S is an illegal included structure." v)))
               (:PRINT-FUNCTION (setq print-function v))
	       (:PRINT-OBJECT (setq print-object v))
               (:TYPE (setq type v))
               (:INITIAL-OFFSET (setq initial-offset v))
               (t (error "~S is an illegal defstruct option." o))))
            (t
             (if (consp (car os))
                 (setq o (caar os))
                 (setq o (car os)))
             (case o
               (:CONSTRUCTOR
                (setq constructors
                      (cons default-constructor constructors)))
	       (:CONC-NAME
		(setq conc-name nil))
               ((:COPIER :PREDICATE :PRINT-FUNCTION :PRINT-OBJECT))
               (:NAMED (setq named t))
               (t (error "~S is an illegal defstruct option." o))))))

    ;; Skip the documentation string.
    (when (and (not (endp slot-descriptions))
               (stringp (car slot-descriptions)))
          (setq documentation (car slot-descriptions))
          (setq slot-descriptions (cdr slot-descriptions)))

    ;; check initial-offset and type consistency.
    (when initial-offset
      (unless type
        (error "Structure definition for ~a cannot have :INITIAL-OFFSET without :TYPE."
               name)))

    ;; :named and type consistency.
    (when named
      (unless type
        (error "Structure definition for ~a cannot have :NAMED without :TYPE."
               name)))

    ;; :print-object or :print-function and type consistency.
    (when (and print-object print-function)
      (error "Structure definition for ~a cannot specify both :PRINT-OBJECT and :PRINT-FUNCTION."
             name))
    (when type
      (when print-object
        (error "Structure definition for ~a cannot specify both :TYPE and :PRINT-OBJECT." name))
      (when print-function
        (error "Structure definition for ~a cannot specify both :TYPE and :PRINT-FUNCTION." name)))

    (when initial-offset (setq size initial-offset))
    (when named
      (unless (or (subtypep '(vector symbol) type env)
                  (subtypep type 'list env))
        (error "Structure cannot have type ~S and be :NAMED." type))
      (setq name-offset size)
      (setq size (1+ size)))

    ;; Parse slot-descriptions, incrementing OFFSET for each one.
    (do ((ds slot-descriptions (cdr ds))
         (sds nil))
        ((endp ds)
         (setq slot-descriptions (nreverse sds)))
      (push (parse-slot-description (car ds)) sds)
      (setq size (1+ size)))

    ;; If TYPE structure is named,
    ;;  add the slot for the structure-name to the slot-descriptions.
    (when named
      (setq slot-descriptions
            (cons (parse-slot-description (list 'typed-structure-name name)) slot-descriptions)))

    ;; Pad the slot-descriptions with the initial-offset number of NILs.
    (when initial-offset
      (setq slot-descriptions
            (append (make-list initial-offset) slot-descriptions)))

    ;; Append the slot-descriptions of the included structure.
    ;; The slot-descriptions in the include option are also counted.
    #+(or)
    (cond ((null include))
          ((endp (cdr include))
           (setq slot-descriptions
                 (append (structure-slot-descriptions (car include))
                         slot-descriptions)))
          (t
           (setq slot-descriptions
                 (append (overwrite-slot-descriptions
                          (mapcar #'(lambda (sd)
                                      (parse-slot-description sd :unknown))
                                  (cdr include))
                          (structure-slot-descriptions (car include)))
                         slot-descriptions))))

    (cond (no-constructor
           ;; If a constructor option is NIL,
           ;;  no constructor should have been specified.
           (when constructors
                 (error "Contradictory constructor options.")))
          ((null constructors)
           ;; If no constructor is specified,
           ;;  the default-constructor is made.
           (setq constructors (list default-constructor))))

    (dolist (constructor constructors)
      ;; a "standard constructor" is one with no specified lambda list, taking &key instead.
      ;; (In this macroexpander, constructors is a list of things, and each thing is either a
      ;;  symbol or a list; the former means a standard constructor, the latter has a lambda
      ;;  list as its second element.)
      ;; Standard constructors are used by #s and so must be stored specially.
      (when (symbolp constructor)
        (setq standard-constructor constructor)))

    ;; Check the named option and set the predicate.
    (when (and type (not named))
      (when (and predicate-specified (not (null predicate)))
	(error "Cannot specify :TYPE and a PREDICATE but not :NAMED, in structure definition for ~a"
	       name))
      (setq predicate nil))

    (when include
      (setq overwriting-slot-descriptions
            (mapcar (lambda (sd)
                      (parse-slot-description sd :unspecified))
                    (cdr include)))
      (setq include (car include)))

    ;;
    ;; The constructors rely on knowing the structure class. For toplevel
    ;; forms we can use LOAD-TIME-VALUE. For non-toplevel forms, we can not
    ;; as the class might be defined _after_ the system decides to evaluate
    ;; LOAD-TIME-VALUE.
    ;;
    ;; In Cleavir/Clasp the LOAD-TIME-VALUEs may be evaluated before ANY
    ;; toplevel forms in the file - so we can't depend on ANY toplevel forms
    ;; to define values required by LOAD-TIME-VALUEs
    ;;
    `(progn
       ,(cond ((null type)
               `(define-class-struct ,name ,conc-name ,include ,slot-descriptions
                  ,overwriting-slot-descriptions ,print-function ,print-object
                  ,constructors ,predicate ,copier ,documentation))
              ((subtypep type 'list env)
               `(define-list-struct ,name ,conc-name ,include ,slot-descriptions
                  ,overwriting-slot-descriptions ,name-offset
                  ,constructors ,predicate ,copier))
              ((subtypep type 'vector env)
               `(define-vector-struct ,name ,conc-name ,type ,include ,slot-descriptions
                  ,overwriting-slot-descriptions ,name-offset
                  ,constructors ,predicate ,copier))
              (t
               (error "~a is not a valid :TYPE in structure definition for ~a"
                      type name)))
       ,@(expand-set-documentation name 'structure documentation)
       ,@(when (and documentation *keep-documentation*)
           `((set-documentation ',name 'structure ',documentation)))
       (setf (structure-constructor ',name) ',standard-constructor)
       ',name)))
