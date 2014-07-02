;;
;; Insert the compiler into the repl
;;
;; Don't use FORMAT here use BFORMAT 
;; otherwise you will have problems when format.lsp is bootstrapped

(in-package :cmp)

  
(setq *implicit-compile-hook*
      (compile nil '(lambda (form &optional environment)
                     (multiple-value-bind (compiled-function warn fail)
                         (compile-in-env nil `(lambda () ,form) environment)
                       (values compiled-function warn fail)))))

