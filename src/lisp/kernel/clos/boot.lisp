;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.o
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CLOS")

#+(or)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *echo-repl-read* t))

;;; ----------------------------------------------------------------------
;;; Building basic classes.
;;; We have to work carefully because the system is obviously not yet
;;; self-consistent.
;;; Some classes, like standard-class, are partially defined in the
;;; interpreter.

#+(or)
(defmacro debug-boot (msg &rest args)
  `(format t ,msg ,@args))
(defmacro debug-boot (msg &rest args)
  (declare (ignore msg args)))


;; This ensures that each new class has its class-for-instances set
;; properly, and furthermore that it's only done once
;; (Because if ensure-boot-class is called again with the same name,
;;  it'll just find-class the existing one.)
(defun allocate-boot-class (metaclass slot-count name)
  (let ((class (core:allocate-standard-instance metaclass slot-count)))
    (core:class-new-stamp class name)
    class))

(defun ensure-boot-class (name &key (metaclass 'standard-class)
                                 direct-superclasses direct-slots)
  (debug-boot "!!ensure-boot-class for ~s metaclass: ~s direct-superclasses: ~s :direct-slots ~s~%"
              name metaclass direct-superclasses direct-slots)
  (let* ((the-metaclass (progn
                          (debug-boot "    About to do the~%")
                          (find-class metaclass nil)))
         (class (progn
                  (debug-boot "    About to allocate-boot-class~%")
                  (or (find-class name nil)
                      (allocate-boot-class the-metaclass #.(length +standard-class-slots+) name)))))
    ;;    (debug-boot "About to with-early-accessors -> macroexpand = ~a~%" (macroexpand '(with-early-accessors (+standard-class-slots+) (setf (class-name                  class) name))))
    (debug-boot "    About to with-early-accessors~%")
    (with-early-accessors (+standard-class-slots+)
      (let ((existing-slots (class-slots class)))
        (when (and (typep existing-slots 'list)
                   (not (= (length existing-slots)
                           (length direct-slots)))
                   (not (zerop (length existing-slots))))
          (error "~S was called on the already instantiated class ~S, but with ~S slots while it already has ~S slots."
                 'ensure-boot-class name (length direct-slots) (length existing-slots))))
      ;;      (debug-boot "  (get-setf-expansion '(class-name class) ENV) -> ~a~%" (macrolet ((hack (form &environment e) `',(multiple-value-list (get-setf-expansion form e)))) (hack '(class-name class))))
      (setf (class-name                 class) name)
      (debug-boot "    (class-name class) -> ~a    name -> ~a~%" (class-name class) name)
      ;; FIXME: This duplicates the :initform specifications in hierarchy.lisp.
      (setf (specializer-direct-methods class) nil
            (specializer-call-history-generic-functions class) nil
            (specializer-mutex class) (mp:make-shared-mutex 'call-history-generic-functions-mutex)
            (class-name                 class) name
            ;; superclasses below
            (class-direct-subclasses   class) nil
            ;; slots by add-slots below
            ;; precedence below
            ;; direct-slots also by add-slots
            (class-direct-default-initargs class) nil
            (class-default-initargs    class) nil
            (class-finalized-p         class) t
            (class-source-position     class) nil)
      (debug-boot "    About to setf class name -> ~a  class -> ~a~%" name class)
      (core:setf-find-class class name)
      (debug-boot "    Done setf class name -> ~a  class -> ~a~%" name class)
      (setf
       (class-dependents          class) nil)
      (debug-boot "      About to add-slots~%")
      (add-slots class direct-slots)
      (debug-boot "      About to get superclasses~%")
      (let ((superclasses (loop for name in direct-superclasses
                             for parent = (find-class name)
                             do (pushnew class (class-direct-subclasses parent))
                             collect parent)))
        (debug-boot "      Collected superclasses~%")
        (setf (class-direct-superclasses class) superclasses)
        ;; In clasp each class contains a default allocator functor
        ;; that is used to allocate instances of this class
        ;; If a superclass is derived from a C++ adaptor class
        ;; then we must inherit its allocator
        ;; This means that a class can only ever
        ;; inherit from one C++ adaptor class
        (setf (creator class) (sys:compute-instance-creator class the-metaclass superclasses))
        (debug-boot "      compute-clos-class-precedence-list  class->~a   superclasses->~a~%" class superclasses)
        (let ((cpl (compute-clos-class-precedence-list class superclasses)))
          (debug-boot "      computed")
          (setf (class-precedence-list class) cpl)))
      class)))

(defun add-slots (class slots)
  (declare (optimize speed (safety 0)))
  ;; It does not matter that we pass NIL instead of a class object,
  ;; because CANONICAL-SLOT-TO-DIRECT-SLOT will make simple slots.
  (with-early-accessors (+standard-class-slots+
			 +slot-definition-slots+)
    (let* ((location-table (make-hash-table :size (if slots 24 0)))
           (direct-slot-class (find-class 'standard-direct-slot-definition nil))
	   (direct-slots (loop for slotd in slots
                               collect (apply #'make-simple-direct-slotd direct-slot-class slotd)))
           (effective-slot-class (find-class 'standard-effective-slot-definition nil))
	   (effective-slots (loop for i fixnum from 0
                                  for slotd in slots
                                  for name = (getf slotd :name)
                                  for declared-location of-type (or null fixnum)
                                    = (getf slotd :location)
                                  for s = (apply #'make-simple-slotd effective-slot-class slotd)
                                  do (setf (slot-definition-location s) i
                                           (gethash name location-table) i)
                                  ;; do a sanity check on :location
                                  when (and declared-location
                                            (/= i (the fixnum declared-location)))
                                    do (error "BUG: Primitive slot ~a has incorrect :location"
                                              name)
                                  collect s)))
      (setf (class-slots class) effective-slots
	    (class-direct-slots class) direct-slots
	    (class-size class) (length slots))
      (setf (class-location-table class) location-table))))


;; Create the classes
;;
(progn
    (defvar +the-t-class+)
    (defvar +the-class+)
    (defvar +the-std-class+)
    (defvar +the-funcallable-standard-class+))

(defmacro dbg-boot (fmt &rest fmt-args)
  (declare (ignore fmt fmt-args))
  nil)

#++
(defmacro dbg-boot (fmt &rest fmt-args)
  `(core:fmt t ,fmt ,@fmt-args))


(defmacro boot-hierarchy ()
  `(progn
     ,@(loop for (class . options) in +class-hierarchy+
          for direct-slots = (getf options :direct-slots)
;;;          do (core:fmt t "boot-hierarchy  class->{}%N" class)
          collect
            (if direct-slots
                `(apply #'ensure-boot-class ',class
                        :direct-slots ,(parse-slots direct-slots)
                        ',(let ((copy (copy-list options)))
                               (remf copy :direct-slots)
                               copy))
                `(apply #'ensure-boot-class ',class ',options)))))

(boot-hierarchy)

(progn
  (dbg-boot "About to setq stuff%N")
  (setq +the-t-class+ (find-class 't nil))
  (setq +the-class+ (find-class 'class nil))
  (setq +the-std-class+ (find-class 'std-class nil))
  (setq +the-funcallable-standard-class+
        (find-class 'funcallable-standard-class nil)))

;; Undefine the SEQUENCE type expansion, which does not account
;; for extended sequences. Redefining SEQUENCE like this is a KLUDGE.
(setf (ext:type-expander 'sequence) nil)

;;
;; Finalize
;;
;; This is needed so that the early slotds we made are not marked obsolete.
;;
(let ()
  (with-early-accessors (+standard-class-slots+)
    (loop for (class-name) in +class-hierarchy+
          for class = (find-class class-name)
          do (loop for s in (class-slots class)
                   do (si:instance-sig-set s))
             (loop for s in (class-direct-slots class)
                   do (si:instance-sig-set s)))))
