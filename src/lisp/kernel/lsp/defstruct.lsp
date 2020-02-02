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

;;; We parse DEFSTRUCT slot descriptions into something like DEFCLASS syntax,
;;; that is, a list (slot-name ...plist)
;;; Valid plist keys are :initform, :initarg, :type, :reader, and :accessor,
;;; and they work as in DEFCLASS.
;;; :initarg is included because any :named position is treated as a slot,
;;; except that its initialization cannot be customized.
;;; Part of the idea here is we make things independent of conc-name
;;; (and thereby, interning) before we do much susbstantial processing.

(defun error-defstruct-slot-syntax (slot-description)
  (error "~a is not a valid DEFSTRUCT slot specification." slot-description))

(defun defstruct-accessor-name (conc-name slot-name)
  ;; NOTE: No conc-name is not the same as a conc-name of "",
  ;; because in the first case the symbol could be in a different package.
  (if conc-name
      (intern (base-string-concatenate conc-name slot-name))
      slot-name))

(defun parse-slot-description (slot-description conc-name)
  (let ((tail nil) slot-name read-only)
    (cond ((symbolp slot-description)
           (setq slot-name slot-description))
          ((consp slot-description)
           (setq slot-name (car slot-description))
           (cond ((null (cdr slot-description)))
                 ((consp (cdr slot-description))
                  (setq tail (list* :initform (cadr slot-description) tail))
                  (do ((os (cddr slot-description) (cddr os))
                       (seen-read-only nil) (seen-type nil))
                      ((endp os))
                    (case (car os)
                      (:type
                       (if seen-type
                           (error-defstruct-slot-syntax slot-description)
                           (setq seen-type t
                                 tail (list* :type (cadr os) tail))))
                      (:read-only
                       (if seen-read-only
                           (error-defstruct-slot-syntax slot-description)
                           (setq seen-read-only t
                                 ;;; treat :read-only nil correctly
                                 read-only (cadr os))))
                      (otherwise
                       (error-defstruct-slot-syntax slot-description)))))
                 (t (error-defstruct-slot-syntax slot-description))))
          (t (error-defstruct-slot-syntax slot-description)))
    ;; Finally, add access and initarg.
    (list* slot-name
           (if read-only :reader :accessor)
           (defstruct-accessor-name conc-name slot-name)
           :initarg
           (intern (symbol-name slot-name) "KEYWORD")
           tail)))

;;; Make the slot description for the :named option.
;;; It's treated as a slot so that child structures can initialize it simply.
(defun named-slot-description (structure-type-name)
  ;; The slot name is an uninterned symbol, guaranteeing no conflict with
  ;; user slots or included :named slots.
  `(,(copy-symbol structure-type-name) :initform ',structure-type-name))

;;; Convenience function for use with mapcar.
(defun slot-description-parser (conc-name)
  (lambda (slot-description)
    (parse-slot-description slot-description conc-name)))

;;; UNPARSE-SLOT-DESCRIPTION does the opposite, turning one of the above into
;;;  something that would work in DEFSTRUCT.
;;; This is for documentation purposes only (describe uses it) at the moment,
;;;  and it should probably remain this way.

(defun unparse-slot-description (list)
  (let ((slot-name (car list)) (plist (cdr list)) (default (list nil)))
    (let ((initform (getf plist :initform default))
          (read-only (getf plist :reader nil))
          (type (getf plist :type t)))
      (if (eq initform default) ; no initform; simple specification
          slot-name
          `(,slot-name ,initform :read-only ,read-only :type ,type)))))

;;; Apply an :INCLUDE slot override.
(defun override-slotd (slot-name over-plist old-plist)
  (destructuring-bind (&key (initform nil initformp)
                         (type t typep) initarg
                         reader accessor)
      over-plist
    (let ((old-reader (getf old-plist :reader))
          (old-accessor (getf old-plist :accessor)))
      (when (and accessor old-reader)
        #+(or)(error "Mutable slot ~a cannot override read-only included slot."
                     slot-name)
        (setq reader accessor accessor nil))
      `(,slot-name
        ;; We always have an initarg for any non-:NAMED slot.
        :initarg ,initarg
        ,@(when initformp `(:initform ,initform))
        ;; NOTE: Could check it's a subtype.
        ,@(let* ((default (list nil))
                 (old-type (getf old-plist :type default)))
            (cond (typep `(:type ,type))
                  ((not (eq old-type default))
                   `(:type ,old-type))))
        ,@(cond
            (reader
             ;; Bug #881: Don't define accessor functions redundantly.
             (unless (eq reader (or old-reader old-accessor))
               `(:reader ,reader)))
            (accessor
             (unless (eq accessor old-accessor)
               `(:accessor ,accessor))))))))

;;; Replace the :reader or :accessor in an old slotd with a new name.
(defun fix-old-slotd (conc-name old-slotd)
  (destructuring-bind (slot-name &key (initform nil initformp)
                                   (type t typep) initarg
                                   reader accessor)
      old-slotd
    (let ((accname (defstruct-accessor-name conc-name slot-name)))
      `(,slot-name :initarg ,initarg
                   ,@(when initformp `(:initform ,initform))
                   ,@(when typep `(:type ,type))
                   ,@(cond
                       (reader
                        ;; Bug #881 again.
                        (unless (eq accname reader)
                          `(:reader ,accname)))
                       (accessor
                        (unless (eq accname accessor)
                          `(:accessor ,accname))))))))

;;; Given defstruct slot-descriptions from both the given defstruct
;;; and an included parent, return a final list of descriptions.
;;; This means removing redundant accessors, checking for duplicates,
;;; getting new accessor names, and just appending.
;;; FIXME: An obscure point in DEFSTRUCT is not defining accessor functions
;;; already defined by an :include-d definition. Bug #881 covers the case
;;; when the same slot is implicated in both definitions, but it's possible
;;; for an unrelated slot to imply the same accessor names as a parent
;;; definition, and we don't handle that correctly.
(defun final-slot-descriptions (conc-name new-slotds over-slotds old-slotds)
  (let ((output nil) (old-slotds (copy-list old-slotds)))
    ;; Apply overrides to old slots.
    (do ((old-slotds old-slotds (rest old-slotds)))
        ((endp old-slotds))
      (let* ((old-slotd (first old-slotds))
             (slot-name (first old-slotd))
             (over-slotd (first (member slot-name over-slotds
                                        :key #'first))))
        (setf (first old-slotds)
              (cond ((null old-slotd) nil)
                    (over-slotd
                     (override-slotd
                      slot-name (rest over-slotd) (rest old-slotd)))
                    (t (fix-old-slotd conc-name old-slotd))))))
    ;; Signal an error for any override wtih nothing to override.
    (dolist (over-slotd over-slotds)
      (let ((slot-name (first over-slotd)))
        (unless (member slot-name old-slotds :key #'first)
          (error "Cannot override nonexistent slot ~a" slot-name))))
    ;; Signal an error for any duplicate slot.
    (dolist (new-slotd new-slotds)
      ;;; check for "dummy" slotdescriptions coming from :initial-offset
      (unless (null new-slotd)
        (let ((slot-name (first new-slotd)))
          (when (member slot-name old-slotds :key #'first)
            (error "Duplicate slot ~a" slot-name)))))
    ;; Done.
    (append old-slotds new-slotds)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; %%DEFSTRUCT is like DEFSTRUCT, but with easier to work with syntax.
;;; Additionally, inclusion has been resolved, so the list of slots is analogous
;;; to the effective slots in CLOS/MOP (rather than the direct).
;;; This is what does the work of DEFSTRUCT.
;;;

(defun defstruct-slotd->defclass-slotd (slot-description)
  (destructuring-bind (slot-name &key (initform nil initformp) initarg (type t)
                       &allow-other-keys)
      slot-description
    `(,slot-name ,@(when initformp `(:initform ,initform))
                 :initarg ,initarg :type ,type)))

(defun defstruct-class-reader-body (structure-name element-type location)
  (declare (ignore element-type))
  `(if (typep object ',structure-name)
       (si:instance-ref object ,location)
       (error 'type-error
              :datum object
              :expected-type ',structure-name)))

(defun defstruct-class-writer-body (structure-name element-type location)
  (declare (ignore element-type))
  `(if (typep object ',structure-name)
       (progn (si:instance-set object ,location new)
              new)
       (error 'type-error
              :datum object
              :expected-type ',structure-name)))

(defun defstruct-vector-reader-body (structure-name element-type location)
  (declare (ignore element-type) ; maybe later.
           (ignore structure-name))
  `(row-major-aref object ,location))

(defun defstruct-vector-writer-body (structure-name element-type location)
  (declare (ignore element-type structure-name))
  `(setf (row-major-aref object ,location) new))

(defun defstruct-list-reader-body (structure-name element-type location)
  (declare (ignore structure-name element-type))
  `(nth ,location object))

(defun defstruct-list-writer-body (structure-name element-type location)
  (declare (ignore structure-name element-type))
  `(setf (nth ,location object) new))

(defun gen-defstruct-accessor (structure-name element-type slotd location
                               gen-read gen-write otype)
  (destructuring-bind (slot-name &key (type t) reader accessor
                       &allow-other-keys)
      slotd
    (multiple-value-bind (accessor read-only)
        (if accessor (values accessor nil) (values reader t))
      (unless (not accessor) ; no reader OR accessor
        (list*
         `(declaim (ftype (function (,otype) ,type) ,accessor)
                   (inline ,accessor))
         `(defun ,accessor (object)
            ,(funcall gen-read structure-name element-type location))
         (unless read-only
           (list
            `(declaim (ftype (function (,type ,otype) ,type) (setf ,accessor))
                      (inline (setf ,accessor)))
            `(defun (setf ,accessor) (new object)
               ,(funcall gen-write structure-name element-type location)))))))))

(defun process-boa-lambda-list (original-lambda-list slot-descriptions)
  (let ((lambda-list (copy-list original-lambda-list))
        ;; list of slotds that will be initialized by the lambda list.
        (mentioned-slots nil)
        ;; list of slotds that need set forms in the body.
        (initialized-slots nil)
        ;; whether the lambda list contains &aux. Also represents
        ;; the state of having seen &aux.
        (aux nil))
    ;; Generate a lambda list handler and immediately discard it,
    ;; to ensure syntactic correctness.
    ;; FIXME: Use an actual lambda list parser for all this.
    ;; We can't use the normal one since it puts in NIL initforms.
    (make-lambda-list-handler lambda-list nil 'function)
    (do* ((sublist lambda-list (rest sublist))
          (name (first sublist) (first sublist))
          ;; Whether we should modify the lambda list to insert a default initform.
          (modify nil))
         ((endp sublist))
      (cond ((or (eq name '&optional) (eq name '&key)) (setq modify t))
            ((eq name '&rest) (setq modify nil))
            ;; modify nil because "&aux x" in a BOA lambda list means we
            ;; don't have to initialize, even if there is an initform.
            ((eq name '&aux) (setq modify nil aux t))
            ((eq name '&allow-other-keys))
            ((atom name) ; just a variable.
             (let ((slotd (assoc name slot-descriptions)))
               (when slotd
                 ;; FIXME: check for duplicated names?
                 ;; or does lambda list handler catch that.
                 (push slotd mentioned-slots)
                 (unless aux
                   (push slotd initialized-slots))
                 (when modify
                   ;; We use a default NIL initform instead of conditionally
                   ;; initializing, for similar reasons to the kw case below.
                   (let ((initform (getf (rest slotd) :initform)))
                     (setf (first sublist)
                           `(,name ,initform)))))))
            (t ; complex parameter.
             (let* ((slot-name (if (consp (first name)) ; complicated :key
                                   (second (first name))
                                   (first name)))
                    (slotd (assoc slot-name slot-descriptions)))
               (when slotd
                 (push slotd mentioned-slots)
                 (if (endp (rest name)) ; like &optional (x)
                     (unless aux
                       (setf (rest name)
                             (list (getf (rest slotd) :initform)))
                       (push slotd initialized-slots))
                     (push slotd initialized-slots)))))))
    ;; OK, we have our lambda list set up... except anything with an :initform
    ;; that wasn't mentioned needs to be added (as &aux).
    (let ((more-aux nil) (default (list nil)))
      (dolist (slotd (set-difference slot-descriptions mentioned-slots))
        (let ((slot-name (first slotd))
              (initform (getf (rest slotd) :initform default)))
          (unless (eq default initform)
            (push `(,slot-name ,initform) more-aux)
            (push slotd initialized-slots))))
      ;; Actual return values!
      (values
       ;; The lambda list.
       (nconc lambda-list (if aux nil (list '&aux)) more-aux)
       ;; Slots to initialize.
       initialized-slots))))

(defun defstruct-constructor-def (name original-lambda-list
                                  slot-descriptions alloc genset)
  (multiple-value-bind (lambda-list initialized-slots)
      (process-boa-lambda-list original-lambda-list slot-descriptions)
    (let ((osym (gensym "NEW"))
          (forms nil))
      ;; Construct initialization forms.
      (do ((index 0 (1+ index))
           (slot-descriptions slot-descriptions (rest slot-descriptions)))
          ((null slot-descriptions))
        (let ((slotd (first slot-descriptions)))
          (when (member slotd initialized-slots)
            (push (funcall genset osym (first slotd) index) forms))))
      ;; Done
      `(defun ,name ,lambda-list
         (let ((,osym ,alloc))
           ,@forms
           ,osym)))))

(defun defstruct-kw-constructor-def (name slot-descriptions alloc genset)
  (let ((kwparams nil) (aux nil) (forms nil)
        (osym (gensym "NEW")))
    (do ((index 0 (1+ index))
         (slot-descriptions slot-descriptions (cdr slot-descriptions)))
        ((null slot-descriptions))
      (let ((slotd (first slot-descriptions)))
        (unless (null slotd) ; spacer
          (let* ((slot-name (first slotd))
                 ;; we use uninterned symbols as parameters per CLHS defstruct:
                 ;; "The symbols which name the slots must not be used by the
                 ;;  implementation as the names for the lambda variables in the
                 ;;  constructor function, since one or more of those
                 ;;  symbols might have been proclaimed special or..."
                 ;; NOTE: In the BOA case, the programmer digs their own hole.
                 (var (copy-symbol slot-name))
                 (initarg (getf (rest slotd) :initarg))
                 ;; I don't think we'd save any time by checking a
                 ;; -p variable and not initializing, so we just
                 ;; initialize slots to NIL if they aren't passed
                 ;; and have no initform.
                 (initform (getf (rest slotd) :initform)))
            (if initarg
                ;; keyword (normal) argument
                (push (list (list initarg var) initform) kwparams)
                ;; aux argument - :named structure names only
                ;; (so, incidentally, there's always an initform)
                (push (list var initform) aux))
            (push (funcall genset osym var index) forms)))))
    `(defun ,name (&key ,@kwparams)
       (let (,@aux
             (,osym ,alloc))
         ,@forms
         ,osym))))

(defun defstruct-dispatch-constructor-def (option slot-descriptions alloc genset)
  (if (eq (first option) :constructor)
      (defstruct-constructor-def (second option) (third option)
                                 slot-descriptions alloc genset)
      (defstruct-kw-constructor-def (second option) slot-descriptions alloc genset)))

(defun defstruct-class-option-expander (structure-name slot-descriptions)
  (lambda (option)
    (case (car option)
      ((:constructor :kw-constructor)
       (defstruct-dispatch-constructor-def
        option slot-descriptions
        `(allocate-instance
          ;; The class is not immediately available at l-t-v time-
          ;; because the defclass form must be evaluated first.
          ;; Thus, bullshit.
          (let ((class (load-time-value (list nil))))
            (or (car class)
                (car (rplaca class (find-class ',structure-name))))))
        (lambda (obj var loc) `(si:instance-set ,obj ,loc ,var))))
      ((:print-function :print-object)
       (let ((obj (gensym "OBJ")) (stream (gensym "STREAM")))
         `(defmethod print-object ((,obj ,structure-name) ,stream)
            (,(second option) ,obj ,stream
             ,@(when (eq (car option) :print-function) '(0))))))
      ((:predicate)
       `(defgeneric ,(second option) (object)
          (:method (object) nil)
          (:method ((object ,structure-name)) t)))
      ((:copier)
       ;; It might seem like we can do better here- basically copy
       ;; a fixed number of slots - but CLHS is clear that the copier
       ;; must be COPY-STRUCTURE, and so it has to deal correctly with subclasses.
       `(defun ,(second option) (instance) (copy-structure instance)))
      ((:documentation)))))

(defun defstruct-vector-option-expander
    (structure-name element-type included-size slot-descriptions)
  (lambda (option)
    (case (first option)
      ((:constructor :kw-constructor)
       (defstruct-dispatch-constructor-def
        option slot-descriptions
        `(make-array ,(length slot-descriptions)
                     :element-type ',element-type)
        (lambda (obj var loc)
          `(setf (row-major-aref ,obj ,loc) ,var))))
      ((:predicate)
       (let ((pred (second option)) (name-loc (third option)))
         `(defun ,(second option) (object)
            (and (typep object '(simple-array ,element-type (*)))
                 (>= (length object) ,(length slot-descriptions))
                 (eq (row-major-aref object ,(+ name-loc included-size))
                     ',structure-name)))))
      ((:copier)
       `(defun ,(second option) (instance) (copy-seq instance)))
      ((:documentation)
       `(set-documentation ',structure-name 'structure
                           ',(second option))))))

(defun named-slot-description-p (structure-name slot-description)
  (let* ((nsd (named-slot-description structure-name))
         (sym1 (first slot-description))
         (sym2 (first nsd)))
    (and (symbolp sym1)(symbolp sym2)(string= (symbol-name sym1)(symbol-name sym2))
         (equalp (rest nsd) (rest slot-description)))))
                                 
(defun defstruct-list-option-expander
    (structure-name included-size slot-descriptions)
  (lambda (option)
    (case (first option)
      ((:constructor :kw-constructor)
       ;; FIXME: inefficient
       (defstruct-dispatch-constructor-def
        option slot-descriptions
        `(make-list ,(length slot-descriptions))
        (lambda (obj var loc)
          `(setf (nth ,loc ,obj) ,var))))
      ((:predicate)
       `(defun ,(second option) (object)
          (and
           (consp object)
           ;; need to test before object is changed with setf below
           ;; FIXME: inefficient
           (eq (nth ,(+ (third option) included-size) object)
               ',structure-name)
           ,@(let (forms)
               (dolist (sd slot-descriptions forms)
                 ;;; if the structure is :named, the first slot-decription is for the name and should not enter the following list
                 ;;; need to recognize a named-slot-description
                 (unless (named-slot-description-p structure-name sd)
                   (push '(consp (setf object (cdr object))) forms)))))))
      ((:copier)
       `(defun ,(second option) (instance) (copy-list instance)))
      ((:documentation)
       `(set-documentation ',structure-name 'structure
                           ',(second option))))))

(defun defstruct-option-expander (name type-base element-type
                                  included-size slot-descriptions)
  (case type-base
    (structure-object
     (defstruct-class-option-expander name slot-descriptions))
    (vector (defstruct-vector-option-expander
                name element-type included-size slot-descriptions))
    (list (defstruct-list-option-expander
              name included-size slot-descriptions))))

(defmacro %%defstruct (name type (include included-size)
                       (&rest slot-descriptions)
                       &rest options)
  (multiple-value-bind (type-base element-type)
      ;; NOTE about :type. CLHS says the structure :TYPE "must be one of"
      ;; LIST, VECTOR, or (VECTOR element-type). Nothing about subtypes
      ;; or expanding deftypes or whatever. We used to use SUBTYPEP here
      ;; but this is simpler and apparently in line with the standard.
      (cond ((null type) 'structure-object)
            ((eq type 'list) type)
            ((eq type 'vector) (values 'vector t))
            ((and (consp type) (eq (car type) 'vector)
                  (consp (cdr type)) (null (cddr type)))
             (values 'vector (second type)))
            (t (error "~a is not a valid :TYPE in structure definition for ~a"
                      type name)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (structure-type ',name) ',type-base
               (structure-slot-descriptions ',name) ',slot-descriptions))
       ,@(when (eq type-base 'structure-object)
           `((defclass ,name ,(if include (list include) nil)
               (,@(mapcar #'defstruct-slotd->defclass-slotd slot-descriptions))
               ,@(let ((doc (second (assoc :documentation options))))
                   (when doc `((:documentation ,doc))))
               (:metaclass structure-class))))
       ,@(let ((result nil))
           (multiple-value-bind (gen-read gen-write otype)
               (case type-base
                 (structure-object (values #'defstruct-class-reader-body
                                           #'defstruct-class-writer-body
                                           name))
                 (vector (values #'defstruct-vector-reader-body
                                 #'defstruct-vector-writer-body
                                 `(simple-array
                                   ,element-type
                                   (,(length slot-descriptions)))))
                 (list (values #'defstruct-list-reader-body
                               #'defstruct-list-writer-body
                               'list)))
             (do ((slotds slot-descriptions (rest slotds))
                  (location 0 (1+ location)))
                 ((endp slotds) result)
               (when (first slotds) ; skip filler
                 (setq result (nconc (gen-defstruct-accessor
                                      name element-type
                                      (first slotds) location
                                      gen-read gen-write otype)
                                     result))))))
       ,@(mapcar (defstruct-option-expander name type-base element-type
                   included-size slot-descriptions)
                 options)
       ,@(let ((kwcon (second (assoc :kw-constructor options))))
           (when kwcon
             `((setf (structure-constructor ',name) ',kwcon))))
       ',name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; %DEFSTRUCT is like DEFSTRUCT, but with easier to work with syntax.
;;; Once any INCLUDE is available, this expands into %%DEFSTRUCT, with an
;;; explicit list of slots, and all interning done.
;;; %DEFSTRUCT forms are macroexpansions of the actual DEFSTRUCT macro,
;;; in which syntax has been checked already.
;;;

(defmacro %defstruct ((name conc-name) type include
                      (&rest overriding-slot-descriptions)
                      (&rest slot-descriptions)
                      &rest options)
  (cond ((null include)
         `(%%defstruct ,name ,type (,include 0)
                       (,@slot-descriptions)
                       ,@options))
        ((names-structure-p include) ; normal include case
         (let* ((old (structure-slot-descriptions include))
                (slotds (final-slot-descriptions
                         conc-name slot-descriptions
                         overriding-slot-descriptions
                         old)))
           `(%%defstruct ,name ,type (,include ,(length old))
                         (,@slotds)
                         ,@options)))
        (t ; include not defined yet
         ;; FIXME: It's nonconforming to err here -
         ;; the included class could be defined later.
         (error-missing-include name include))))

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
    (let (type include overriding-slot-descs conc-name seen-conc-name
          overriding-slot-descriptions
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
                   (if seen-conc-name
                       (error "Specified ~a more than once" :conc-name)
                       (setq conc-name (if (null second)
                                           nil
                                           (string second))
                             seen-conc-name t)))
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
                   (setq overriding-slot-descriptions rest))
                  (otherwise
                   (error-unknown-defstruct-option opt-name))))
              (let ((opt-name (if (consp option) (car option) option)))
                (case opt-name
                  ((:constructor)
                   (push (default-constructor-name name) kw-constructors))
                  ((:conc-name)
                   (if seen-conc-name
                       (error "Specified ~a more than once" :conc-name)
                       (setq conc-name nil seen-conc-name t)))
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
          ;;; This option takes one argument, which specifies the name of the type predicate. 
          ;;; If the argument is provided and is nil, no predicate is defined.
          ;;; If the argument is not supplied or if the option itself is not supplied,
          ;;; the name of the predicate is made by concatenating the name of the structure to the string "-P",
          ;;; interning the name in whatever package is current at the time defstruct is expanded. 
          (unless (or predicate seen-predicate)
            (setq predicate (default-predicate-name name))))
      ;; default conc-name
      (unless seen-conc-name
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
      ;; Parse overriding slot descriptions
      (setq overriding-slot-descriptions
            (mapcar (slot-description-parser conc-name) overriding-slot-descriptions))

      (values name type include overriding-slot-descriptions
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
    (let ((slot-descriptions slots) name-offset documentation
          standard-constructor)
      ;; Skip the documentation string.
      (when (and (not (endp slot-descriptions))
                 (stringp (car slot-descriptions)))
        (setq documentation (car slot-descriptions))
        (setq slot-descriptions (cdr slot-descriptions)))

      ;; A specialized vector can't have a name if symbols can't be put in.
      (when named
        (unless (or (subtypep '(vector symbol) type env)
                    (subtypep type 'list env))
          (error "Structure cannot have type ~S and be :NAMED." type))
        (setq name-offset (or initial-offset 0)))

      ;; Parse slot-descriptions.
      (setq slot-descriptions
            (mapcar (slot-description-parser conc-name) slot-descriptions))

      ;; If TYPE structure is named,
      ;;  add the slot for the structure-name to the slot-descriptions.
      (when named
        (setq slot-descriptions
              (cons (named-slot-description name) slot-descriptions)))

      ;; Pad the slot-descriptions with the initial-offset number of NILs.
      (when initial-offset
        (setq slot-descriptions
              (append (make-list initial-offset) slot-descriptions)))

      (unless (null kw-constructors)
        ;; a "standard constructor" is one with no specified lambda list, taking &key instead.
        ;; Standard constructors are used by #s and so must be stored specially.
        ;; We take the first one defined, arbitrarily. (Usually there will be at most one.)
        (setq standard-constructor (first kw-constructors)))

      `(%defstruct (,name ,conc-name) ,type ,include
         (,@overriding-slot-descriptions)
         (,@slot-descriptions)
         ,@(mapcar (lambda (constructor) `(:constructor ,@constructor))
                   constructors)
         ,@(mapcar (lambda (kwcon) `(:kw-constructor ,kwcon))
                   kw-constructors)
         ,@(when print-function `((:print-function ,print-function)))
         ,@(when print-object `((:print-object ,print-object)))
         ,@(when predicate `((:predicate ,predicate ,name-offset)))
         ,@(when copier `((:copier ,copier)))
         ,@(when (and documentation *keep-documentation*)
             `((:documentation ,documentation)))))))
