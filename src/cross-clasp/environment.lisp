(in-package #:cross-clasp)

(defvar *build-rte*)
(defvar *build-ce*)

(defclass client (maclina.vm-cross:client) ())

(defclass reader-client (maclina.compile-file::reader-client) ())

(defvar *reader-client* (make-instance 'reader-client))

(define-condition substituting-package (warning)
  ((%package-name :reader substituting-package :initarg :substituting))
  (:report (lambda (condition stream)
             (format stream "Unknown package ~s; substituting CORE"
                     (substituting-package condition)))))

(defmethod maclina.compile-file::find-package ((client reader-client) package-name)
  (or (clostrum:find-package m:*client* *build-rte* package-name)
    (warn 'substituting-package :substituting package-name)
    (cl:find-package "CROSS-CLASP.CLASP.CORE")))

(defmethod maclina.compile-file::package-name ((client client) env package)
  (or (clostrum:package-name client env package)
    (warn 'substituting-package :substituting package)
    "CORE"))

(defun cross-compile-file (input-file &rest keys)
  (apply #'maclina.compile-file:compile-file input-file
         :environment *build-rte*
         :reader-client *reader-client*
         keys))

(defmethod clostrum-sys:variable-cell :around ((client client)
                                               environment symbol)
  (if (keywordp symbol)
      (let ((cell (clostrum-sys:ensure-variable-cell client environment symbol)))
        (setf (clostrum-sys:variable-cell-value client cell) symbol)
        cell)
      (call-next-method)))

(defmethod clostrum-sys:variable-status :around ((client client)
                                                 environment symbol)
  (if (keywordp symbol)
      :constant
      (call-next-method)))

(defun fdesignator (designator)
  (etypecase designator
    (function designator)
    (symbol (clostrum:fdefinition m:*client* *build-rte* designator))))

(defun macroexpand-hook ()
  (fdesignator
   (maclina.machine:symbol-value m:*client* *build-rte*
                                 '*macroexpand-hook*)))

(defun build-macroexpand-1 (form &optional env)
  (extrinsicl:macroexpand-1 m:*client* (or env *build-rte*)
                            (macroexpand-hook) form))
(defun build-macroexpand (form &optional env)
  (extrinsicl:macroexpand m:*client* (or env *build-rte*)
                          (macroexpand-hook) form))

(defun describe-variable (symbol &optional env)
  (trucler:describe-variable m:*client* (or env *build-rte*) symbol))

(defun constantp (form &optional env)
  (typecase form
    (symbol (let ((info (describe-variable form env)))
              (typecase info
                (trucler:constant-variable-description t)
                ;; could put symbol macros here
                (t nil))))
    (cons ; could expand this obviously
     (case (first form)
       ((quote) t)
       (t nil)))
    (t t)))

(defun constant-form-value (form &optional env)
  (let ((env (or env *build-rte*)))
    (typecase form
      (symbol (let ((info (describe-variable form env)))
                (etypecase info
                  (trucler:constant-variable-description
                   (trucler:value info)))))
      (cons
       (ecase (first form)
         ((quote) (second form))))
      (t form))))

(defun find-compiler-class (name)
  (clostrum:find-class m:*client* *build-rte* name t))

(defun gf-info (name)
  ;; stuffed into inline data for now
  (let ((dat (clostrum:operator-inline-data m:*client* *build-rte*
                                            name)))
    (etypecase dat
      ((or null clos::compiler-generic) dat)
      (t (error "Not a generic: ~s" name)))))
