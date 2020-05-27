(in-package #:static-gfs)

;;;; NOTE: This file is compiled/loaded before CLOS is up.

#|
We use a lazy strategy for constructor cells. That is, things like finalize-inheritance
that render constructors invalid only mark them, and the actual computation of a new
constructor is only done when MAKE-INSTANCE is actually called.
This may seem unintuitive - why not be aggressive? The reason is that if we re/load a
file full of class and method definitions, we don't want to clog up the works with a
ton of unnecessary constructor recompilations. It's not possible (as far as I can think)
to give a way to the compiler to figure out when a class is "finished" enough that
a constructor ought to be computed, before make-instance.
|#

;;; MAPPING

(defvar *constructor-cells* (make-hash-table :test #'eq))

(defun make-invalid-cell (class-designator keys)
  (let ((cell (make-constructor-cell class-designator keys)))
    (setf (cell-function cell) (invalidated-constructor cell))
    cell))

(defmacro ensure-gethash (key table &optional default)
  (let ((valueg (gensym "VALUE"))
        (presentpg (gensym "PRESENTP"))
        (keyo (gensym "KEY"))
        (tableo (gensym "TABLE")))
    `(let ((,keyo ,key) (,tableo ,table))
       (multiple-value-bind (,valueg ,presentpg)
           (gethash ,keyo ,tableo)
         (if ,presentpg
             ,valueg
             (setf (gethash ,keyo ,tableo) ,default))))))

(defun ensure-designator-table (class-designator)
  (ensure-gethash class-designator *constructor-cells*
                  (make-hash-table :test #'equal)))

(defun ensure-constructor-cell (class-designator keys)
  (ensure-gethash keys (ensure-designator-table class-designator)
                  (make-invalid-cell class-designator keys)))

;;; used in precompile
(defun force-constructor (class-designator keys function)
  (setf (cell-function
         (ensure-constructor-cell class-designator keys))
        function))

;;; debug
(defun find-constructor-cell (class-designator keys)
  (multiple-value-bind (table presentp)
      (gethash class-designator *constructor-cells*)
    (if presentp
        (gethash keys table)
        (values nil nil))))

;;; debug
(defun designated-constructors ()
  (let (designators)
    (maphash (lambda (designator table)
               (declare (ignore table))
               (push designator names))
             *constructor-cells*)
    designators))

;;; debug
(defun constructor-cells (designator)
  (let ((cells nil))
    (maphash (lambda (key cell)
               (declare (ignore key))
               (push cell cells))
             (or (gethash designator *constructor-cells*)
                 (return-from constructor-cells nil)))
    cells))

(defun map-constructor-cells (function designator)
  (let ((table (gethash designator *constructor-cells*)))
    (when table
      (maphash (lambda (key value)
                 (declare (ignore key))
                 (funcall function value))
               table))))

(defun proper-class-name (class)
  (let ((name (class-name class)))
    (when (and name (eq (find-class name nil) class))
      name)))

(defun invalidate-cell (cell)
  (setf (cell-function cell) (invalidated-constructor cell)))

(defun invalidate-designated-constructors (designator)
  (map-constructor-cells #'invalidate-cell designator))

(defun invalidate-class-constructors (class)
  (invalidate-designated-constructors class)
  (let ((name (proper-class-name class)))
    (when name (invalidate-designated-constructors name))))

;;; CONSTRUCTORS
;;; These are no-compile ones- "constructing" them is just allocating a closure,
;;; so there's no chance of recursion (unless closures use make-instance, eheh).
;;; The real constructors are built in compute-constructor.

(defun invalidated-constructor (cell)
  (lambda (&rest args)
    (declare (core:lambda-name invalidated-constructor))
    ;; Defined in compute-constructor.lisp
    (update-constructor-cell cell)
    ;; Alternately could just use make-instance.
    (apply cell args)))

;;; Temporary constructor put in place in case compute-constructor ends up
;;; calling make-instance (e.g. in the compiler).
;;; Obviously it's strictly worse than not inlining would have been, but
;;; it's temporary.
(defun fallback-constructor (designator keys)
  (lambda (&rest args)
    (declare (core:lambda-name fallback-constructor))
    (declare (notinline make-instance))
    (apply #'make-instance designator
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
