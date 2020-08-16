(in-package #:static-gfs)

;;; Make definitions of x-initialize methods update constructors.
;;; NOTE: If a constructor cell is in the unfinalized state, most of this could
;;; be skipped. But I don't think making method redefinitions more efficient
;;; is worth the complexity.

;;; We use this proxy object instead of just making cells dependents of the
;;; functions. We don't want possibly-thousands of cells cluttering up
;;; dependents lists, plus this way we can more easily update only those cells
;;; that need it.

(defclass cell-updater () ())

;; See KLUDGE in fixup.lisp.
(locally
    (declare (notinline make-instance))
  (let ((updater (make-instance 'cell-updater)))
    (clos:add-dependent #'make-instance updater)
    (clos:add-dependent #'initialize-instance updater)
    (clos:add-dependent #'shared-initialize updater)))

;; FIXME?: With diamond inheritance will do redundant work.
;; Doesn't matter with invalidate-cell though.
(defun map-class-and-subclass-constructor-cells (function class)
  (let ((name (proper-class-name class)))
    (when name
      (map-constructor-cells function name)))
  (loop for subclass in (clos:class-direct-subclasses class)
        do (map-class-and-subclass-constructor-cells function subclass)))

(defun invalidate-class-and-subclass-constructor-cells (class)
  (map-class-and-subclass-constructor-cells
   #'invalidate-cell class))

(defmethod clos:update-dependent
    ((f (eql #'make-instance)) (updater cell-updater) &rest initargs)
  ;; I don't quite understand what a make-instance method would be doing.
  ;; Also they're rare, so I don't mind just updating everything.
  (declare (ignore initargs))
  (invalidate-class-and-subclass-constructor-cells (find-class 'standard-object)))

(defun update-dependent-with-initargs (initargs)
  (destructuring-bind (&optional key method &rest more) initargs
    (declare (ignore more))
    (when (or (eq key 'cl:add-method) (eq key 'cl:remove-method))
      ;; For the functions we're interested in, the first argument
      ;; is the class.
      (let ((class (first (clos:method-specializers method))))
        (invalidate-class-and-subclass-constructor-cells class)))))

(defmethod clos:update-dependent
    ((f (eql #'initialize-instance)) (updater cell-updater) &rest initargs)
  (update-dependent-with-initargs initargs))

(defmethod clos:update-dependent
    ((f (eql #'shared-initialize)) (updater cell-updater) &rest initargs)
  (update-dependent-with-initargs initargs))
