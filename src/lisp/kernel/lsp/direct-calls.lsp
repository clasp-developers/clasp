;;;
;;; This can only be compiled by bclasp or cclasp
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (core:select-package "CORE"))

;;;If the c-name symbol is available then compile a direct call defun to that symbol

(defmacro wrap-c++-function (raw-lisp-name declare-forms lambda-list c-name)
  (unless (and (consp raw-lisp-name)
               (eq (car raw-lisp-name) 'core:magic-intern))
    (error "Only magic-intern is supported"))
  (multiple-value-bind (pkg-sym pkg sym)
      (apply 'core:magic-name (cdr raw-lisp-name))
    (declare (ignore pkg-sym))
    (let ((lisp-name (find-symbol sym pkg)))
      (if (and (not (declared-global-inline-p lisp-name))
               (dlsym :rtld-default c-name)
               (fboundp lisp-name))
          `(multiple-value-bind (pathname pos lineno column)
               ;; Preserve source info - we want the original C++ definitions,
               ;; not the wrappers
               (core:function-source-pos (fdefinition ',lisp-name))
             (defun ,lisp-name ,lambda-list
               (declare (optimize (debug 0)))
               ,@(if declare-forms
                     (list `(declare ,@declare-forms))
                     nil)
               (core:multiple-value-foreign-call ,c-name ,@(core:names-of-lexical-variables
                                                            (core:make-lambda-list-handler
                                                             lambda-list nil 'function))))
             (core:set-source-pos-info (fdefinition ',lisp-name)
                                       pathname pos lineno column))
          `(unless core:*silent-startup*
             (bformat t "Will not generate wrapper for %s - the symbol is not available or set up for CL inlining%N" ',lisp-name))))))

;; identical to the above but with a setf name. FIXME cleanliness, but magic-intern is pretty magical.
(defmacro wrap-c++-function-setf (raw-lisp-name declare-forms lambda-list c-name)
  (unless (and (consp raw-lisp-name)
               (eq (car raw-lisp-name) 'core:magic-intern))
    (error "Only magic-intern is supported"))
  (multiple-value-bind (pkg-sym pkg sym)
      (apply 'core:magic-name (cdr raw-lisp-name))
    (declare (ignore pkg-sym))
    (let ((lisp-name (list 'cl:setf (find-symbol sym pkg))))
      (if (and (not (declared-global-inline-p lisp-name))
               (dlsym :rtld-default c-name)
               (fboundp lisp-name))
          `(multiple-value-bind (pathname pos lineno column)
               (core:function-source-pos (fdefinition ',lisp-name))
             (defun ,lisp-name ,lambda-list
               (declare (optimize (debug 0)))
               ,@(if declare-forms
                     (list `(declare ,@declare-forms))
                     nil)
               (core:multiple-value-foreign-call ,c-name ,@(core:names-of-lexical-variables
                                                            (core:make-lambda-list-handler
                                                             lambda-list nil 'function))))
             (core:set-source-pos-info (fdefinition ',lisp-name)
                                       pathname pos lineno column))
          `(unless core:*silent-startup*
             (bformat t "Will not generate wrapper for %s - the symbol is not available or set up for CL inlining%N" ',lisp-name))))))
