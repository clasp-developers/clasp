(in-package #:static-gfs)

;;; MAPPING

(defvar *constructor-cells* (make-hash-table :test #'eq))

(defun ensure-constructor-cell (class-name keys)
  (let ((table (ensure-gethash class-name *constructor-cells*
                               (make-hash-table :test #'equal))))
    ;; See fallback-constructor note for why we don't just
    ;; ensure-gethash here.
    (multiple-value-bind (cell presentp) (gethash keys table)
      (if presentp
          cell
          (let ((new (make-cell class-name keys
                                (fallback-constructor class-name keys))))
            (setf (gethash keys table) new)
            (update-constructor-cell new)
            new)))))

;;; debug
(defun find-constructor-cell (class-name keys)
  (multiple-value-bind (table presentp)
      (gethash class-name *constructor-cells*)
    (if presentp
        (gethash keys table)
        (values nil nil))))

;;; debug
(defun constructor-cells (name)
  (let ((cells nil))
    (maphash (lambda (key cell)
               (declare (ignore key))
               (push cell cells))
             (or (gethash name *constructor-cells*)
                 (return-from constructor-cells nil)))
    cells))

(defun update-constructor-cell (cell)
  (setf (cell-function cell)
        (compute-constructor (cell-name cell) (cell-keys cell))))

(defun map-constructor-cells (function name)
  (let ((table (gethash name *constructor-cells*)))
    (when table
      (maphash (lambda (key value)
                 (declare (ignore key))
                 (funcall function value))
               table))))

(defun update-constructors (name)
  (map-constructor-cells #'update-constructor-cell name))

(defun update-class-and-subclass-constructors (class)
  (let ((name (class-name class)))
    (when name (update-constructors name))
    (mapcar #'update-class-and-subclass-constructors
            (clos:class-direct-subclasses class))))

;;; debug
(defun clear-constructors (name)
  (remhash name *constructor-cells*))

;;; THE BUSINESS

(defun compute-constructor (class-name keys)
  ;; We try to be aggressive and compute a constructor immediately.
  (let ((class (find-class class-name nil)))
    (if class
        ;; FIXME: Better types?
        (etypecase class
          (structure-class (structure-class-constructor class))
          (built-in-class (built-in-class-constructor class))
          (class
           #+(or) ; can't do this if finalize-inheritance updates.
           (clos::finalize-unless-forward class)
           (if (clos:class-finalized-p class)
               ;; bclasp-compile because cclasp is full of make-instance
               (cmp:bclasp-compile nil (constructor-form class keys))
               (unfinalized-constructor class))))
        (undefined-constructor class-name))))

;;; Temporary constructor put in place in case compute-constructor ends up
;;; calling make-instance (e.g. in the compiler).
;;; Obviously it's strictly worse than not inlining would have been, but
;;; it's temporary.
(defun fallback-constructor (name keys)
  (lambda (&rest args)
    (declare (core:lambda-name fallback-constructor))
    (declare (notinline make-instance))
    (apply #'make-instance name
           (loop for key in keys for arg in args
                 collect key collect arg))))

(defun structure-class-constructor (class)
  (lambda (&rest args)
    (declare (core:lambda-name structure-class-constructor))
    (declare (ignore args))
    (error "The structure-class ~a cannot be instantiated" class)))

(defun built-in-class-constructor (class)
  (lambda (&rest args)
    (declare (core:lambda-name built-in-class-constructor))
    (declare (ignore args))
    (error "The built-in-class ~a cannot be instantiated" class)))

(defun undefined-constructor (name)
  (lambda (&rest args)
    (declare (core:lambda-name undefined-constructor))
    (declare (ignore args))
    (error 'ext:undefined-class :name name)))

(defun unfinalized-constructor (class)
  (lambda (&rest args)
    (declare (core:lambda-name unfinalized-constructor))
    (clos:finalize-inheritance class)
    ;; finalize-inheritance should update the cell.
    ;; We say this call's a loss and use the runtime.
    (apply #'make-instance class args)))

(defun make-params (keys)
  (loop for key in keys
        collect (make-symbol (symbol-name key))))

;; aesthetic
(defun constructor-name (class)
  (make-symbol (format nil "OPTIMIZED-~a-CONSTRUCTOR" (class-name class))))

(defun constructor-form (class keys)
  (let ((params (make-params keys)))
    `(lambda (,@params)
       (declare (core:lambda-name ,(constructor-name class)))
       (declare (ignorable ,@params)) ; e.g. duplicate initargs
       ,(make-instance-form class keys params))))
