;;;
;;; This can only be compiled by bclasp or cclasp
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (core:select-package "CORE"))

;;;If the c-name symbol is available then compile a direct call defun to that symbol

(eval-when (:compile-toplevel :execute)
  (defmacro generate-direct-call-defun (raw-lisp-name lambda-list c-name)
    (unless (and (consp raw-lisp-name)
                 (eq (car raw-lisp-name) 'core:magic-intern))
      (error "Only magic-intern is supported"))
    (multiple-value-bind (pkg-sym pkg sym)
        (apply 'core:magic-name (cdr raw-lisp-name))
      (let ((lisp-name (find-symbol sym pkg)))
        (if (and (not (declared-global-inline-p lisp-name)) (dlsym c-name))
            `(defun ,lisp-name ,lambda-list
               (core:intrinsic-call ,c-name ,@(core:names-of-lexical-variables
                                               (core:make-lambda-list-handler
                                                lambda-list nil 'function))))
            `(bformat t "Could not generate wrapper for %s - the symbol is not available\n" ',lisp-name))
        #+(or)(progn
                `(bformat t "Not exposing %s to debug crash in evalmacros\n" ',lisp-name))))))

