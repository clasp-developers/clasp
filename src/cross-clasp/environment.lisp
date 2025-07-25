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
  (or
    (let* ((package (m:symbol-value m:*client* *build-rte* '*package*))
           (local-nicknames
             (trivial-package-local-nicknames:package-local-nicknames package)))
      (cdr (assoc package-name local-nicknames :test #'string=)))
    (clostrum:find-package m:*client* *build-rte* package-name)
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

(defun ext:type-expander (specifier &optional env)
  (let ((env (trucler:global-environment
              m:*client* (or env *build-rte*))))
    (clostrum:type-expander m:*client* env specifier)))
(defun (setf ext:type-expander) (expander specifier &optional env)
  (declare (ignore env))
  (setf (clostrum:type-expander m:*client* *build-rte* specifier)
        expander))

(defun typexpand-1 (type-specifier &optional env)
  (let ((expander
          (etypecase type-specifier
            (symbol (ext:type-expander type-specifier env))
            (cons (ext:type-expander (first type-specifier) env))
            (class nil))))
    (if expander
        (values (funcall expander type-specifier env) t)
        (values type-specifier nil))))

(defun typexpand (type-specifier &optional env)
  (multiple-value-bind (expansion expandedp)
      (typexpand-1 type-specifier env)
    (if expandedp
        (values (typexpand expansion env) t)
        (values type-specifier nil))))

(defun core::normalize-type (type &optional env)
  (let ((type (typexpand type env)))
    (if (consp type)
        (values (first type) (rest type))
        (values type nil))))

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

(defun find-compiler-class (name &optional (errorp t))
  (clostrum:find-class m:*client* *build-rte* name errorp))

(defun core::class-info (name &optional env)
  (let ((env (trucler:global-environment
              m:*client* (or env *build-rte*))))
    (not (not (clostrum:find-class m:*client* env name nil)))))

(defun gf-info (name)
  ;; stuffed into inline data for now
  (let ((dat (clostrum:operator-inline-data m:*client* *build-rte*
                                            name)))
    (etypecase dat
      ((or null clos::compiler-generic) dat)
      (t (error "Not a generic: ~s" name)))))
