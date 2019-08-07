(in-package #:static-gfs)


(defvar *compute-constructor-calls* (ext:make-atomic-fixnum 0))

;;; This function is called when an actual make-instance call is happening.
;;; So it should return something immediately valid or error.
(defun compute-constructor (class-name keys)
  ;; NOTE: For recursion reasons, this function MUST NOT return invalidated-constructor.
  (core:atomic-fixnum-incf-unsafe *compute-constructor-calls*)
  (let ((class (find-class class-name nil)))
    (if class
        ;; FIXME: Better types?
        (etypecase class
          (structure-class (structure-class-constructor class))
          (built-in-class (built-in-class-constructor class))
          (class
           (unless (clos:class-finalized-p class)
             ;; finalize-inheritance will signal an error if it can't be done,
             ;; but again, we call this when make-instance is in progress-
             ;; it SHOULD signal an error.
             (clos:finalize-inheritance class))
           ;; bclasp-compile because cclasp is full of make-instance
           (cmp:bclasp-compile nil (constructor-form class keys))))
        (undefined-constructor class-name))))

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

(defun update-constructor-cell (cell)
  ;; In case compute-constructor ends up calling the cell indirectly,
  ;; we put something in to prevent recursion.
  (let ((name (cell-name cell)) (keys (cell-keys cell)))
    (setf (cell-function cell) (fallback-constructor name keys))
    (setf (cell-function cell) (compute-constructor name keys))))

(defun update-constructors (name)
  (map-constructor-cells #'update-constructor-cell name))

(defun update-class-and-subclass-constructors (class)
  (let ((name (class-name class)))
    (when name (update-constructors name))
    (mapcar #'update-class-and-subclass-constructors
            (clos:class-direct-subclasses class))))

;;; For the user - compile constructors ahead of time.
(defmacro precompile-constructor (class-name keys)
  `(force-constructor ',class-name ',keys
                      ,(constructor-form (find-class class-name) keys)))
