(in-package #:static-gfs)

;;; Make definitions of x-initialize methods update constructors.
;;; NOTE: If a constructor cell is in the unfinalized state, most of this could
;;; be skipped. But I don't think making method redefinitions more efficient
;;; is worth the complexity.

;;; Defining methods on SIMPLE-VECTOR is technically okay since it's a CL class
;;; and thus users can't define methods - but ewwwww. FIXME, figure out built in
;;; funcallable instances or something...

(defmethod clos:update-dependent
    ((f (eql #'make-instance)) (cell simple-vector) &rest initargs)
  ;; I don't quite understand what a make-instance method would be doing.
  ;; Also they're rare, so I don't mind just updating everything.
  (declare (ignore initargs))
  (update-constructor-cell cell))

(defmethod clos:update-dependent
    ((f (eql #'initialize-instance)) (cell simple-vector) &rest initargs)
  (let ((class (find-class (cell-name cell) nil)))
    (when class
      (destructuring-bind (&optional key method &rest more) initargs
        (declare (ignore more))
        (when (and (or (eq key 'cl:add-method) (eq key 'cl:remove-method))
                   (method-may-specialize-using-classes-p
                    method (list class)))
          (update-constructor-cell cell))))))

(defmethod clos:update-dependent
    ((f (eql #'initialize-instance)) (cell simple-vector) &rest initargs)
  (let ((class (find-class (cell-name cell) nil)))
    (when class
      (destructuring-bind (&optional key method &rest more) initargs
        (declare (ignore more))
        (when (and (or (eq key 'cl:add-method) (eq key 'cl:remove-method))
                   (method-may-specialize-using-classes-p
                    method (list class (find-class 't))))
          (update-constructor-cell cell))))))

(defun method-may-specialize-using-classes-p (method classes)
  (loop for class in classes
        for spec in (clos:method-specializers method)
        do (typecase spec
             ;; SUBTYPEP is always certain since these are classes.
             (class (when (not (subtypep class spec))
                      (return nil)))
             (clos:eql-specializer
              ;; Who knows?
              ;; Note that if a shared-initialize method has (eql t)
              ;; this means all cells will be updated.
              (return t))))
  t)
