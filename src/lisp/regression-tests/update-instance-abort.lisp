;;;; Handling errors in UPDATE-INSTANCE-FOR-REDEFINED/DIFFERENT-CLASS
;;;;
;;;; As a slight extension, when an error in user-provided methods on these
;;;; functions is signaled, Clasp rolls back the instance into validity
;;;; before any nonlocal exit completes. See clos/change.lsp.

;;;; This test file is copied with modifications from
;;;; SBCL's mop-32 and mop-33.impure.lisp.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

;;(in-package #:clasp-tests)

;;; UPDATE-INSTANCE-FOR-REDEFINED-CLASS

(define-condition uifrc-failure (error) ())

(defclass uifrc-foo-class (standard-class) ())
(defmethod clos:validate-superclass ((c uifrc-foo-class) (s standard-class)) t)

(defclass uifrc-foo-object (standard-object) ())

(defmethod shared-initialize :around ((class uifrc-foo-class) slot-names
                                      &rest rest
                                      &key direct-superclasses)
  (apply #'call-next-method
         class slot-names
         :direct-superclasses
         (append (remove (find-class 'standard-object) direct-superclasses)
                 (list (find-class 'uifrc-foo-object)))
         rest))

(defmethod update-instance-for-redefined-class :before
    ((instance uifrc-foo-object)
     added-slots discarded-slots
     property-list
     &rest initargs)
  (declare (ignore added-slots discarded-slots property-list initargs))
  (error 'uifrc-failure))

(defclass uifrc-foo () () (:metaclass uifrc-foo-class))

(defparameter *uifrc-foo* (make-instance 'uifrc-foo))

;;; Redefine UIFRC-FOO, causing *FOO* to become obsolete.
(defclass uifrc-foo ()
  ((slot :initform 42))
  (:metaclass uifrc-foo-class))

;;; The error should have been signaled by the UIFRC method.
(test-expect-error uifrc.abort.1
                   (slot-value *uifrc-foo* 'slot)
                   :type uifrc-failure)
;;; If the update-instance was correctly aborted, this should result in
;;; the same behavior and the same error being signaled.
(test-expect-error uifrc.abort.2
                   (slot-value *uifrc-foo* 'slot)
                   :type uifrc-failure)

;;; Redefine the UIFRC method to no longer signal an error.
(defmethod update-instance-for-redefined-class :before
    ((instance uifrc-foo-object)
     added-slots discarded-slots
     property-list
     &rest initargs)
  (declare (ignore added-slots discarded-slots property-list initargs)))

;;; The instance is now updatable, so make sure that happens.
(test uifrc.abort.3 (slot-value *uifrc-foo* 'slot)
      (42))

;;; UPDATE-INSTANCE-FOR-DIFFERENT-CLASS

(define-condition uifdc-failure (error) ())

(defclass uifdc-foo () ())

(defclass uifdc-bar () ((slot :initform 42)))

(defmethod update-instance-for-different-class :before
    ((foo uifdc-foo) (bar uifdc-bar) &rest initargs)
  (declare (ignore initargs))
  ;; This U-I-F-D-C is meant to always signal an error.
  (error 'uifdc-failure))

;;; Make an instance of FOO.
(defparameter *uifdc-foo* (make-instance 'uifdc-foo))

(test-expect-error uifdc.abort.1 (change-class *uifdc-foo* 'uifdc-bar)
                   :type uifdc-failure)
(test-type uifdc-abort.2 *uifdc-foo* uifdc-foo)

;;; This should *also* result in an "expected failure" error, because after
;;; the previous U-I-F-D-C call made a non-local exit, the instance should be
;;; automatically restored to its previous class.
(test-expect-error uifdc.abort.3 (change-class *uifdc-foo* 'uifdc-bar)
                   :type uifdc-failure)
(test-type uifdc-abort.4 *uifdc-foo* uifdc-foo)

;;; Redefine the U-I-F-D-C method to no longer signal an error.
(defmethod update-instance-for-different-class :before
    ((foo uifdc-foo) (bar uifdc-bar) &rest initargs)
  (declare (ignore initargs)))

;;; It is now possible to change the instance's class
;;; and access the new and newly initialized slot.
(test uifdc.abort.5 (progn (change-class *uifdc-foo* 'uifdc-bar)
                           (slot-value *uifdc-foo* 'slot))
      (42))
(test-type  uifdc.abort.6 *uifdc-foo* uifdc-bar)
