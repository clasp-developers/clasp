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

(defun make-invalid-cell (class-name keys)
  (let ((cell (make-constructor-cell class-name keys)))
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

(defun ensure-name-table (class-name)
  (ensure-gethash class-name *constructor-cells*
                  (make-hash-table :test #'equal)))

(defun ensure-constructor-cell (class-name keys)
  (ensure-gethash keys (ensure-name-table class-name)
                  (make-invalid-cell class-name keys)))

;;; used in precompile
(defun force-constructor (class-name keys function)
  (setf (cell-function
         (ensure-constructor-cell class-name keys))
        function))

;;; debug
(defun find-constructor-cell (class-name keys)
  (multiple-value-bind (table presentp)
      (gethash class-name *constructor-cells*)
    (if presentp
        (gethash keys table)
        (values nil nil))))

;;; debug
(defun named-constructors ()
  (let (names)
    (maphash (lambda (name table)
               (declare (ignore table))
               (push name names))
             *constructor-cells*)
    names))

;;; debug
(defun constructor-cells (name)
  (let ((cells nil))
    (maphash (lambda (key cell)
               (declare (ignore key))
               (push cell cells))
             (or (gethash name *constructor-cells*)
                 (return-from constructor-cells nil)))
    cells))

(defun map-constructor-cells (function name)
  (let ((table (gethash name *constructor-cells*)))
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

(defun invalidate-named-constructors (name)
  (map-constructor-cells #'invalidate-cell name))

(defun invalidate-class-constructors (class)
  (let ((name (proper-class-name class)))
    (when name (invalidate-named-constructors name))))

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
