;;;
;;; This can only be compiled by bclasp or cclasp
;;;
(in-package :core)

;;;If the c-name symbol is available then compile a direct call defun to that symbol

#+(or)
(core:fset
 'generate-direct-call-defun
 #'(lambda (form env)
     (let ((raw-lisp-name (cadr def))
           (lambda-list (caddr def))
           (c-name (cadddr def)))
       (if (not (eq (car raw-lisp-name) 'core:magic-intern))
           (error "Only magic-intern is supported"))
       (let ((evaluated-lisp-name (apply 'core:magic-intern (cdr raw-lisp-name))))
         (if (dlsym c-name)
             `(core:fset ',evaluated-lisp-name
                         #'(lambda ,lambda-list
                             (core:intrinsic-call ,c-name ,@(core:names-of-lexical-variables
                                                             (core:make-lambda-list-handler
                                                              lambda-list nil 'function)))))
             `(bformat t "Could not generate wrapper for %s - the symbol is not available\n" ,evaluated-lisp-name)))))
 t #| its a macro |#)

(defmacro generate-direct-call-defun (raw-lisp-name lambda-list c-name)
  (unless (and (consp raw-lisp-name)
               (eq (car raw-lisp-name) 'core:magic-intern))
    (error "Only magic-intern is supported"))
  (let ((evaluated-lisp-name (apply 'core:magic-intern (cdr raw-lisp-name))))
    (if (dlsym c-name)
        `(defun ,evaluated-lisp-name ,lambda-list
           (core:intrinsic-call ,c-name ,@(core:names-of-lexical-variables
                                           (core:make-lambda-list-handler
                                            lambda-list nil 'function))))
        `(bformat t "Could not generate wrapper for %s - the symbol is not available\n" ',evaluated-lisp-name))))

