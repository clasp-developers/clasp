;;;
;;; This can only be compiled by bclasp or cclasp
;;;
(in-package :core)

;;;If the c-name symbol is available then compile a direct call defun to that symbol

(defmacro foo () `(format t "Running direct-calls.lsp~%"))

(foo)

(defmacro generate-direct-call-defun (raw-lisp-name lambda-list c-name)
  (unless (and (consp raw-lisp-name)
               (eq (car raw-lisp-name) 'core:magic-intern))
    (error "Only magic-intern is supported"))
  (multiple-value-bind (pkg-sym pkg sym)
      (apply 'core:magic-name (cdr raw-lisp-name))
    (let ((lisp-name (find-symbol sym pkg)))
      #+(or)(if (dlsym c-name)
                `(defun ,lisp-name ,lambda-list
                   (core:intrinsic-call ,c-name ,@(core:names-of-lexical-variables
                                                   (core:make-lambda-list-handler
                                                    lambda-list nil 'function))))
                `(bformat t "Could not generate wrapper for %s - the symbol is not available\n" ',lisp-name))
      (progn
        `(bformat t "Foo Not exposing %s to debug crash in evalmacros\n" ',lisp-name)))))

