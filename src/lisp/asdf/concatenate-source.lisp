;;;; -------------------------------------------------------------------------
;;;; Concatenate-source

(uiop/package:define-package :asdf/concatenate-source
  (:recycle :asdf/concatenate-source :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
   :asdf/component :asdf/operation
   :asdf/system :asdf/find-system
   :asdf/action :asdf/lisp-action :asdf/bundle)
  (:export
   #:concatenate-source-op
   #:load-concatenated-source-op
   #:compile-concatenated-source-op
   #:load-compiled-concatenated-source-op
   #:monolithic-concatenate-source-op
   #:monolithic-load-concatenated-source-op
   #:monolithic-compile-concatenated-source-op
   #:monolithic-load-compiled-concatenated-source-op))
(in-package :asdf/concatenate-source)

;;;
;;; Concatenate sources
;;;
(with-upgradability ()
  (defclass basic-concatenate-source-op (bundle-op)
    ((bundle-type :initform "lisp")))
  (defclass basic-load-concatenated-source-op (basic-load-op selfward-operation) ())
  (defclass basic-compile-concatenated-source-op (basic-compile-op selfward-operation) ())
  (defclass basic-load-compiled-concatenated-source-op (basic-load-op selfward-operation) ())

  (defclass concatenate-source-op (basic-concatenate-source-op non-propagating-operation) ())
  (defclass load-concatenated-source-op (basic-load-concatenated-source-op)
    ((selfward-operation :initform '(prepare-op concatenate-source-op) :allocation :class)))
  (defclass compile-concatenated-source-op (basic-compile-concatenated-source-op)
    ((selfward-operation :initform '(prepare-op concatenate-source-op) :allocation :class)))
  (defclass load-compiled-concatenated-source-op (basic-load-compiled-concatenated-source-op)
    ((selfward-operation :initform '(prepare-op compile-concatenated-source-op) :allocation :class)))

  (defclass monolithic-concatenate-source-op (basic-concatenate-source-op monolithic-bundle-op non-propagating-operation) ())
  (defclass monolithic-load-concatenated-source-op (basic-load-concatenated-source-op)
    ((selfward-operation :initform 'monolithic-concatenate-source-op :allocation :class)))
  (defclass monolithic-compile-concatenated-source-op (basic-compile-concatenated-source-op)
    ((selfward-operation :initform 'monolithic-concatenate-source-op :allocation :class)))
  (defclass monolithic-load-compiled-concatenated-source-op (basic-load-compiled-concatenated-source-op)
    ((selfward-operation :initform 'monolithic-compile-concatenated-source-op :allocation :class)))

  (defmethod input-files ((operation basic-concatenate-source-op) (s system))
    (loop :with encoding = (or (component-encoding s) *default-encoding*)
          :with other-encodings = '()
          :with around-compile = (around-compile-hook s)
          :with other-around-compile = '()
          :for c :in (required-components
                      s :goal-operation 'compile-op
                        :keep-operation 'compile-op
                        :other-systems (operation-monolithic-p operation))
          :append
          (when (typep c 'cl-source-file)
            (let ((e (component-encoding c)))
              (unless (equal e encoding)
                (let ((a (assoc e other-encodings)))
                  (if a (push (component-find-path c) (cdr a))
                      (push (list a (component-find-path c)) other-encodings)))))
            (unless (equal around-compile (around-compile-hook c))
              (push (component-find-path c) other-around-compile))
            (input-files (make-operation 'compile-op) c)) :into inputs
          :finally
             (when other-encodings
               (warn "~S uses encoding ~A but has sources that use these encodings:~{ ~A~}"
                     operation encoding
                     (mapcar #'(lambda (x) (cons (car x) (list (reverse (cdr x)))))
                             other-encodings)))
             (when other-around-compile
               (warn "~S uses around-compile hook ~A but has sources that use these hooks: ~A"
                     operation around-compile other-around-compile))
             (return inputs)))
  (defmethod output-files ((o basic-compile-concatenated-source-op) (s system))
    (lisp-compilation-output-files o s))

  (defmethod perform ((o basic-concatenate-source-op) (s system))
    (let* ((ins (input-files o s))
           (out (output-file o s))
           (tmp (tmpize-pathname out)))
      (concatenate-files ins tmp)
      (rename-file-overwriting-target tmp out)))
  (defmethod perform ((o basic-load-concatenated-source-op) (s system))
    (perform-lisp-load-source o s))
  (defmethod perform ((o basic-compile-concatenated-source-op) (s system))
    (perform-lisp-compilation o s))
  (defmethod perform ((o basic-load-compiled-concatenated-source-op) (s system))
    (perform-lisp-load-fasl o s)))

