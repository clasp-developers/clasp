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

(defun warn-missing-include (name include)
  ;; FIXME: should be a style warning at most
  (warn "Structure definition for ~a INCLUDEs ~a, unknown at compile time."
        name include))

(defun error-missing-include (name include)
  (error "Cannot define structure ~a - it INCLUDEs ~a, which is undefined."
         name include))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Environment access
;;;

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
  (structure-type name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Miscellaneous

;;; Used by #S reader
(defun make-structure (name initargs)
  (unless (names-structure-p name) (error "~s is not a structure class." name))
  (let ((constructor (structure-constructor name)))
    (if constructor
        (apply constructor initargs)
        (error "The structure class ~s has no standard constructor." name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delaying definitions
;;;
;;; A DEFSTRUCT can :include other structures not defined at compile time
;;; (e.g., from nontoplevel definitions). This is rare and a huge pain and silly
;;; to do since it inhibits the efficiency points of defstruct, but allowed.

(defmacro with-defstruct-delay ((slotds name include slots overwrites env) &body body)
  (declare (ignore env)) ;; see FIXME in environment access
  `(let ((,slotds
           (cond ((null ,include) ; no include
                  ,slots)
                 ((names-structure-p ,include) ; normal include case
                  (append (overwrite-slot-descriptions
                           ,overwrites
                           (structure-slot-descriptions ,include))
                          ,slots))
                 (t ; include not defined yet
                  ;; FIXME: It's nonconforming to err here -
                  ;; the included class could be defined later.
                  (error-missing-include ,name ,include)))))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Slot descriptions
;;;

;;; PARSE-SLOT-DESCRIPTION parses a user slot-description
;;;  and returns a list of the form:
;;;        (slot-name initform slot-type read-only)
;;;  which we call a struct-slotd.
;;;  the read-only parameter is a default. It can be nil, true, or :unspecified
;;;   (used for :include overrides)

(defun make-struct-slotd (name initform type read-only)
  (list name initform type read-only))
(defun struct-slotd-name (slotd) (first slotd))
(defun struct-slotd-initform (slotd) (second slotd))
(defun struct-slotd-type (slotd) (third slotd))
(defun struct-slotd-read-only (slotd) (fourth slotd))

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
    (make-struct-slotd slot-name initform slot-type read-only)))

;;; UNPARSE-SLOT-DESCRIPTION does the opposite, turning one of the above into
;;;  something that would work in DEFSTRUCT.
;;; This is for documentation purposes only (describe uses it) at the moment,
;;;  and it should probably remain this way.
;;; Note that we have no way of distinguishing "initform NIL" and "no initform",
;;;  (though Clasp does currently treat these the same)
;;;  one of several reasons this is not exact.

(defun unparse-slot-description (list)
  (let ((name (struct-slotd-name list)) (initform (struct-slotd-initform list))
        (type (struct-slotd-read-only list)) (read-only (struct-slotd-read-only list)))
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
    (let* ((old-slot (struct-slotd-name old-slots))
           (slot-name (struct-slotd-name old-slot))
           (new-slot (first (member slot-name new-slots :key #'car))))
      (if (null new-slot)
          (setf new-slot old-slot)
          (let* ((old-read-only (struct-slotd-read-only old-slot))
                 (new-read-only (struct-slotd-read-only new-slot)))
            (cond ((and (null new-read-only) old-read-only)
                   (error "Tried to turn an included read only slot ~A writable."
                          slot-name))
                  ((eq new-read-only :unspecified)
                   (setf new-read-only old-read-only)))
            (setf new-slot (make-struct-slotd (struct-slotd-name new-slot)
                                              (struct-slotd-initform new-slot)
                                              (struct-slotd-type new-slot)
                                              new-read-only))))
      (push new-slot output))))

(defun struct-reader-name (name conc-name)
  (if conc-name
      (intern (base-string-concatenate conc-name name))
      name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constructor guts
;;; (By far the messiest part of this file.)
;;;

(defun constructor-helper (constructor slot-descriptions)
  (if (consp constructor)
      ;; BOA constructor. See CLHS 3.4.6 for detailed explanation.
      (let* ((constructor-name (first constructor))
             (original-lambda-list (second constructor))
             (lambda-list (copy-list original-lambda-list))
             (mentioned-slots nil)
             (aux nil)) ; whether the lambda list has &aux already.
        ;; Generate a lambda list handler and immediately discard it,
        ;; to ensure syntactic correctness.
        ;; FIXME: Use an actual lambda list parser for all this.
        (make-lambda-list-handler lambda-list nil 'function)
        (do* ((sublist lambda-list (rest sublist))
              (name (first sublist) (first sublist))
              (modify nil)) ; whether we should modify the lambda list here for defaulting.
             ((endp sublist))
          (cond ((or (eq name '&optional) (eq name '&key)) (setq modify t))
                ((eq name '&rest) (setq modify nil))
                ((eq name '&aux) (setq modify nil aux t))
                ((eq name '&allow-other-keys))
                ((atom name) ; just a variable.
                 (let ((slotd (assoc name slot-descriptions)))
                   (when slotd
                     ;; FIXME: check for duplicated names? or does lambda list handler catch that.
                     (push slotd mentioned-slots)
                     (when modify
                       (setf (first sublist)
                             `(,name ,(struct-slotd-initform slotd)))))))
                (t ; has an initform included.
                 (let* ((slot-name (if (consp (first name)) ; complicated :key
                                       (second (first name))
                                       (first name)))
                        (slotd (assoc slot-name slot-descriptions)))
                   (when slotd
                     (push slotd mentioned-slots)
                     (when (and (endp (rest name)) modify) ; like &optional (foo)
                       (setf (rest name) (list (struct-slotd-initform slotd)))))))))
        ;; For all slots not mentioned above, add the initforms as &aux bindings.
        (let ((other-slots (set-difference slot-descriptions mentioned-slots)))
          (values constructor-name
                  (nconc lambda-list (if aux nil (list '&aux))
                         (mapcar (lambda (slotd)
                                   `(,(struct-slotd-name slotd) ,(struct-slotd-initform slotd)))
                                 other-slots))
                  '* ; FIXME: weak ftype
                  (mapcar #'struct-slotd-name slot-descriptions))))
      ;; standard constructor
      ;; we use uninterned symbols as parameters per CLHS defstruct:
      ;; "The symbols which name the slots must not be used by the implementation as the names
      ;;  for the lambda variables in the constructor function, since one or more of those
      ;;  symbols might have been proclaimed special or..."
      ;; In the BOA case, the programmer digs their own hole.
      (let ((parameters nil) (variables nil) (ftype-parameters nil))
        (mapc (lambda (sd)
                (cond ((null sd) (push nil variables))
                      ((eq (struct-slotd-name sd) 'typed-structure-name)
                       (push (struct-slotd-initform sd) variables))
                      (t (let ((param (copy-symbol (struct-slotd-name sd))))
                           (push (list param (struct-slotd-initform sd)) parameters)
                           (push (list (intern (symbol-name param) "KEYWORD")
                                       (struct-slotd-type sd))
                                 ftype-parameters)
                           (push param variables)))))
              slot-descriptions)
        (values constructor
                `(&key ,@(nreverse parameters))
                `(&key ,@ftype-parameters)
                (nreverse variables)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DEFSTRUCT vector
;;;

(defmacro define-vector-struct-constructors (name element-type constructors slotds)
  `(progn
     ,@(mapcan (lambda (constructor)
                 (multiple-value-bind (constructor-name lambda-list ftype-parameters vars)
                     (constructor-helper constructor slotds)
                   (list `(declaim (ftype (function ,ftype-parameters vector)
                                          ,constructor-name))
                         `(defun ,constructor-name ,lambda-list
                            (let ((result (make-array ,(length vars) :element-type ',element-type)))
                              ,@(let ((index 0))
                                  (mapcar (lambda (var)
                                            (prog1 `(setf (row-major-aref result ,index) ,var)
                                              (incf index)))
                                          vars))
                              result)))))
               constructors)))

(defmacro define-vector-struct-accessors (name conc-name element-type slotds)
  (let ((index 0))
    (flet ((one (sd)
             (prog1
                 (if (or (null sd) ; initial-offset padding
                         (eq (struct-slotd-name sd) 'typed-structure-name))
                     nil
                     (destructuring-bind (slot-name initform type read-only) sd
                       (declare (ignore initform))
                       (let* ((accname (struct-reader-name slot-name conc-name))
                              (writer
                                (if read-only
                                    nil
                                    `((define-setf-expander ,accname (object &environment env)
                                        (get-setf-expansion (list 'row-major-aref object ,index) env))))))
                         (list* `(declaim (ftype (function ((vector ,element-type)) ,type) ,accname)
                                          (inline ,accname))
                                `(defun ,accname (instance)
                                   (the ,type (row-major-aref instance ,index)))
                                writer))))
               (incf index))))
      `(progn ,@(mapcan #'one slotds)))))

(defmacro define-vector-struct (name conc-name element-type include slot-descriptions
                                overwriting-slot-descriptions name-offset
                                constructors predicate copier
                                &environment env)
  (let ((element-type (upgraded-array-element-type element-type env)))
    `(progn
       ,@(with-defstruct-delay (slotds name include
                                slot-descriptions overwriting-slot-descriptions env)
           (let* ((included-size (if include (structure-size include) 0))
                  (name-offset (when name-offset (+ included-size name-offset))))
             `((eval-when (:compile-toplevel :load-toplevel :execute)
                 (setf (structure-type ',name) '(vector ,element-type)
                       (structure-size ',name) ,(length slotds)
                       (structure-slot-descriptions ',name) ',slotds))
               ,@(when predicate
                   `((defun ,predicate (object)
                       (and (typep object '(vector ,element-type))
                            (> (length object) ,name-offset)
                            (eq (row-major-aref object ,name-offset) ',name)))))
               (define-vector-struct-constructors ,name ,element-type ,constructors ,slotds)
               (define-vector-struct-accessors ,name ,conc-name ,element-type ,slotds))))
       ,@(when copier
           `((defun ,copier (instance) (copy-seq instance)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DEFSTRUCT list
;;;

(defmacro define-list-struct-constructors (name constructors slotds)
  `(progn
     ,@(mapcan (lambda (constructor)
                 (multiple-value-bind (constructor-name lambda-list ftype-parameters vars)
                     (constructor-helper constructor slotds)
                   (list `(declaim (ftype (function ,ftype-parameters list)
                                          ,constructor-name))
                         `(defun ,constructor-name ,lambda-list (list ,@vars)))))
               constructors)))

(defmacro define-list-struct-accessors (name conc-name slotds)
  (let ((index 0))
    (flet ((one (sd)
             (prog1
                 (if (or (null sd) ; initial-offset padding
                         (eq (struct-slotd-name sd) 'typed-structure-name))
                     nil
                     (destructuring-bind (slot-name initform type read-only) sd
                       (declare (ignore initform))
                       (let* ((accname (struct-reader-name slot-name conc-name))
                              (writer
                                (if read-only
                                    nil
                                    `((define-setf-expander ,accname (object &environment env)
                                        (get-setf-expansion (list 'nth ,index object) env))))))
                         (list* `(declaim (ftype (function (list) ,type) ,accname)
                                          (inline ,accname))
                                `(defun ,accname (instance)
                                   (the ,type (nth ,index instance)))
                                writer))))
               (incf index))))
      `(progn ,@(mapcan #'one slotds)))))

(defmacro define-list-struct (name conc-name include slot-descriptions
                              overwriting-slot-descriptions name-offset
                              constructors predicate copier
                              &environment env)
  `(progn
     ,@(with-defstruct-delay (slotds name include
                              slot-descriptions overwriting-slot-descriptions env)
         (let* ((included-size (if include (structure-size include) 0))
                (name-offset (when name-offset (+ included-size name-offset))))
           `((eval-when (:compile-toplevel :load-toplevel :execute)
               (setf (structure-type ',name) 'list
                     (structure-size ',name) ,(length slotds)
                     (structure-slot-descriptions ',name) ',slotds))
             ,@(when predicate
                 `((defun ,predicate (object)
                     (and
                      (consp object)
                      ,@(let (forms)
                          (dotimes (i name-offset (nreverse forms))
                            (push '(consp (setf object (cdr object))) forms)))
                      (eq (car object) ',name)))))
             (define-list-struct-constructors ,name ,constructors ,slotds)
             (define-list-struct-accessors ,name ,conc-name ,slotds))))
     ,@(when copier
            `((defun ,copier (instance) (copy-list instance))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DEFSTRUCT class (no :type)
;;;

(defun defstruct-sd->defclass-sd (sd)
  (destructuring-bind (name initform type read-only) sd
    (declare (ignore read-only)) ; handled elsewhere.
    `(,name :initform ,initform :type ,type
            :initarg ,(intern (symbol-name name) "KEYWORD"))))

(defmacro define-class-struct-constructors (name constructors slot-descriptions)
  `(progn
     ,@(mapcan (lambda (constructor)
                 (multiple-value-bind (constructor-name lambda-list ftype-parameters vars)
                     ;; this does most of the work. for example, initforms are set up
                     ;; in the lambda list.
                     (constructor-helper constructor slot-descriptions)
                   (list `(declaim (ftype (function ,ftype-parameters ,name)
                                          ,constructor-name))
                         (let ((instance (gensym "INSTANCE"))
                               (index 0))
                           `(defun ,constructor-name ,lambda-list
                              (let ((,instance
                                      (allocate-instance
                                       ;; The class is not immediately available at l-t-v time-
                                       ;; because the defclass form must be evaluated first.
                                       ;; Thus, bullshit.
                                       (let ((class (load-time-value (list nil))))
                                         (or (car class) (car (rplaca class (find-class ',name))))))))
                                ,@(mapcar (lambda (var)
                                            (prog1 `(si:instance-set ,instance ,index ,var)
                                              (incf index)))
                                          vars)
                                ,instance))))))
               constructors)))

(defmacro define-class-struct-accessors (name conc-name slot-descriptions)
  (let ((index 0))
    (flet ((one (sd)
             (destructuring-bind (slot-name initform type read-only) sd
               (declare (ignore initform))
               (prog1
                   (let* ((accname (struct-reader-name slot-name conc-name))
                          (writer
                            (if read-only
                                nil
                                `((defun (setf ,accname) (new object)
                                    (if (typep object ',name)
                                        (si:instance-set object ,index new)
                                        (error 'type-error
                                               :datum object
                                               :expected-type ',name)))))))
                     (list* `(declaim (ftype (function (,name) ,type) ,accname)
                                      (inline ,accname))
                            `(defun ,accname (instance)
                               (if (typep instance ',name)
                                   (the ,type (si:instance-ref instance ,index))
                                   (error 'type-error
                                          :datum instance
                                          :expected-type ',name)))
                            writer))
                 (incf index)))))
      `(progn ,@(mapcan #'one slot-descriptions)))))

(defmacro define-class-struct (name conc-name include slot-descriptions
                               overwriting-slot-descriptions print-function
                               print-object constructors predicate
                               copier documentation
                               &environment env)
  `(progn
     (defclass ,name ,(and include (list include))
       ;; defclass of course does its own overwriting, so we can just leave these be
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
               (,print-object ,obj ,stream)))))
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
         `((declaim (ftype (function (,name) ,name) ,copier)
                    (inline ,copier))
             (defun ,copier (instance) (copy-structure instance))))

     ,@(with-defstruct-delay (all-slots name include
                              slot-descriptions overwriting-slot-descriptions env)
         `((eval-when (:compile-toplevel :load-toplevel :execute)
             (setf (structure-type ',name) 'structure-object
                   (structure-size ',name) ,(length all-slots)
                   (structure-slot-descriptions ',name) ',all-slots))
           (define-class-struct-constructors ,name ,constructors ,all-slots)
           (define-class-struct-accessors ,name ,conc-name ,all-slots)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The DEFSTRUCT macro.
;;;

(defun check-defstruct-option-too-many-args (name extra)
  (unless (null extra)
    (error "Too many options to ~a" name)))

(defun error-defstruct-option-duplicated (name)
  (error "Multiple ~a options to defstruct" name))

(defun error-defstruct-options-incompatible (name1 name2)
  (error "~a and ~a options to defstruct are incompatible" name1 name2))

(defun error-unknown-defstruct-option (name)
  (error "~a is not a valid option to defstruct" name))

(defun default-constructor-name (name)
  (intern (base-string-concatenate "MAKE-" name)))

(defun default-copier-name (name)
  (intern (base-string-concatenate "COPY-" name)))

(defun default-predicate-name (name)
  (intern (base-string-concatenate name "-P")))

(defun parse-overriding-slot-spec (slot-spec)
  (parse-slot-description slot-spec :unspecified))

;;; Given the second of a defstruct, returns values:
;;; name, type, include or NIL, overriding slot specs,
;;; conc-name (normalized to a string), list of constructors with BOAs,
;;; list of keyword-driven constructors, copier name or NIL, ditto predicate,
;;; a boolean indicating whether the structure is named,
;;; print-function name or NIL, ditto print-object, initial-offset or NIL.
;;; Any symbols that need to be interned are interned in *package*.
(defun parse-defstruct-options (name&opts)
  (multiple-value-bind (name options)
      (cond ((consp name&opts)
             (values (car name&opts) (cdr name&opts)))
            ((symbolp name&opts)
             (values name&opts nil))
            (t
             (error "Name of a structure class must be a symbol, not ~a"
                    name&opts)))
    (let (type include overriding-slot-specs conc-name
          constructors kw-constructors no-constructor
          predicate seen-predicate copier seen-copier (named nil)
          print-function print-object initial-offset seen-initial-offset)
      (do ((os options (cdr os)))
          ((endp os))
        (let ((option (car os)))
          (if (and (consp option)
                   (consp (cdr option)))
              (let ((opt-name (car option))
                    (second (cadr option))
                    (rest (cddr option)))
                (case opt-name
                  ((:conc-name)
                   (check-defstruct-option-too-many-args :conc-name rest)
                   (if conc-name
                       (error "Specified ~a more than once" :conc-name)
                       (setq conc-name (if (null second)
                                           ""
                                           (string second)))))
                  ((:constructor)
                   (cond ((null second) ; no constructor
                          (setq no-constructor t))
                         ((null rest) ; keyword constructor
                          (push second kw-constructors))
                         (t ; BOA constructor
                          (let ((boa (first rest)))
                            (check-defstruct-option-too-many-args
                             :constructor (rest rest))
                            (push (list second boa) constructors)))))
                  ((:copier)
                   (check-defstruct-option-too-many-args :copier rest)
                   (if seen-copier
                       (error-defstruct-option-duplicated :copier)
                       (setq seen-copier t))
                   (unless (symbolp second)
                     (error "~a option must specify a symbol" :copier))
                   (setq copier second))
                  ((:predicate)
                   (check-defstruct-option-too-many-args :predicate rest)
                   (if seen-predicate
                       (error-defstruct-option-duplicated :predicate)
                       (setq seen-predicate t))
                   (unless (symbolp second)
                     (error "~a option must specify a symbol" :predicate))
                   (setq predicate second))
                  ((:initial-offset)
                   (check-defstruct-option-too-many-args :initial-offset rest)
                   (if seen-initial-offset
                       (error-defstruct-option-duplicated :initial-offset)
                       (setq seen-initial-offset t))
                   (unless (and (integerp second) (>= second 0))
                     (error "~a option must specify a nonnegative integer"
                            :initial-offset))
                   (setq initial-offset second))
                  ((:print-function)
                   (check-defstruct-option-too-many-args :print-function rest)
                   (when print-function
                     (error-defstruct-option-duplicated :print-function))
                   (if second
                       (setq print-function second)
                       (error "~a option must specify a function name"
                              :print-function)))
                  ((:print-object)
                   (check-defstruct-option-too-many-args :print-object rest)
                   (when print-object
                     (error-defstruct-option-duplicated :print-object))
                   (if second
                       (setq print-object second)
                       (error "~a option must specify a function name, not NIL"
                              :print-object)))
                  ((:type)
                   (check-defstruct-option-too-many-args :type rest)
                   (if type
                       (error-defstruct-option-duplicated :type)
                       (setq type second)))
                  ((:include)
                   (if (null second)
                       (error "NIL is not a valid included structure name")
                       (setq include second))
                   (setq overriding-slot-specs
                         (mapcar #'parse-overriding-slot-spec rest)))
                  (otherwise
                   (error-unknown-defstruct-option opt-name))))
              (let ((opt-name (if (consp option) (car option) option)))
                (case opt-name
                  ((:constructor)
                   (push (default-constructor-name name) kw-constructors))
                  ((:conc-name)
                   (if conc-name
                       (error "Specified ~a more than once" :conc-name)
                       (setq conc-name "")))
                  ((:copier)
                   (if seen-copier
                       (error-defstruct-option-duplicated :copier)
                       (setq seen-copier t))
                   (setq copier (default-copier-name name)))
                  ((:predicate)
                   (if seen-predicate
                       (error-defstruct-option-duplicated :predicate)
                       (setq seen-predicate t))
                   (setq predicate (default-predicate-name name)))
                  ((:print-function :print-object)) ; FIXME: What do these mean...?
                  ((:named)
                   (cond ((consp option)
                          (error "~a was specified but is invalid syntax - it should just be ~a"
                                 option :named))
                         (named
                          (error-defstruct-option-duplicated :named))
                         (t (setq named t))))
                  (otherwise
                   (error-unknown-defstruct-option opt-name)))))))
      ;; We have all the options. Do some final consistency checks,
      ;; and set defaults.
      (if no-constructor
          (unless (and (null constructors) (null kw-constructors))
            (error "~a was specified, but there were other ~a options"
                   '(:constructor nil) :constructor))
          (when (and (null constructors) (null kw-constructors))
            (push (default-constructor-name name) kw-constructors)))
      (when (and (not seen-copier) (null copier))
        (setq copier (default-copier-name name)))
      ;; default predicate + consistency
      (if (and type (not named))
          (when predicate
            (error "Cannot specify :TYPE and a PREDICATE but not :NAMED, in structure definition for ~a"
                   name))
          (unless predicate
            (setq predicate (default-predicate-name name))))
      ;; default conc-name
      (unless conc-name
        (setq conc-name (base-string-concatenate name "-")))
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
        (error-defstruct-options-incompatible :print-object :print-function))
      (when type
        (when print-object
          (error-defstruct-options-incompatible :print-object :type))
        (when print-function
          (error-defstruct-options-incompatible :print-function :type)))
      ;; type and predicate and named consistency
      (when (and type predicate (not named)))

      (values name type include overriding-slot-specs
              conc-name constructors kw-constructors
              copier predicate named
              print-function print-object
              initial-offset))))

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
  (multiple-value-bind (name type include overriding-slot-descriptions
                        conc-name constructors kw-constructors
                        copier predicate named
                        print-function print-object initial-offset)
      (parse-defstruct-options name&opts)
    (let ((slot-descriptions slots) (size 0) name-offset documentation
          standard-constructor)
      ;; Skip the documentation string.
      (when (and (not (endp slot-descriptions))
                 (stringp (car slot-descriptions)))
        (setq documentation (car slot-descriptions))
        (setq slot-descriptions (cdr slot-descriptions)))

      (when initial-offset (setq size initial-offset))

      ;; A specialized vector can't have a name if symbols can't be put in.
      (when named
        (unless (or (subtypep '(vector symbol) type env)
                    (subtypep type 'list env))
          (error "Structure cannot have type ~S and be :NAMED." type))
        (setq name-offset size)
        (setq size (1+ size)))

      ;; Parse slot-descriptions, incrementing SIZE for each one.
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
              (cons (parse-slot-description (list 'typed-structure-name `',name)) slot-descriptions)))

      ;; Pad the slot-descriptions with the initial-offset number of NILs.
      (when initial-offset
        (setq slot-descriptions
              (append (make-list initial-offset) slot-descriptions)))

      (unless (null kw-constructors)
        ;; a "standard constructor" is one with no specified lambda list, taking &key instead.
        ;; Standard constructors are used by #s and so must be stored specially.
        ;; We take the first one defined, arbitrarily. (Usually there will be at most one.)
        (setq standard-constructor (first kw-constructors)))

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
      (let ((constructors (append constructors kw-constructors)))
        `(progn
           ;; NOTE about :type. CLHS says the structure :TYPE "must be one of"
           ;; LIST, VECTOR, or (VECTOR element-type). Nothing about subtypes
           ;; or expanding deftypes or whatever. We used to use SUBTYPEP here
           ;; but this is simpler and apparently in line with the standard.
           ,(cond ((null type)
                   `(define-class-struct ,name ,conc-name ,include ,slot-descriptions
                      ,overriding-slot-descriptions ,print-function ,print-object
                      ,constructors ,predicate ,copier ,documentation))
                  ((eq type 'list)
                   `(define-list-struct ,name ,conc-name ,include ,slot-descriptions
                      ,overriding-slot-descriptions ,name-offset
                      ,constructors ,predicate ,copier))
                  (t
                   (let ((element-type
                           (cond ((eq type 'vector) 't)
                                 ((and (consp type) (null (cddr type)) (second type)))
                                 (t
                                  (error "~a is not a valid :TYPE in structure definition for ~a"
                                         type name)))))
                     `(define-vector-struct ,name ,conc-name ,element-type
                        ,include ,slot-descriptions
                        ,overriding-slot-descriptions ,name-offset
                        ,constructors ,predicate ,copier))))
           ,@(when (and documentation *keep-documentation*)
               `((set-documentation ',name 'structure ',documentation)))
           (setf (structure-constructor ',name) ',standard-constructor)
           ',name)))))
