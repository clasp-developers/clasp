

(in-package :compiler)

;;
;; variable lookups are in this file so we can compile-file it first and make 
;; compilation faster
;;

(defun codegen-special-var-lookup (result sym env)
  "Return IR code that returns the value cell of a special symbol"
  (cmp-log "About to codegen-special-var-lookup symbol[%s] full-name[%s]\n" sym (full-name sym))
  (if (eq sym 'nil)
      (codegen-ltv-nil result env)
      (let ((global-symbol-ptr (irc-global-symbol sym)))
	(cmp-log "About to invoke create-call2 - global-symbol-ptr --> %s\n" global-symbol-ptr)
	(irc-call env "symbolValueRead" result global-symbol-ptr))))




(defun codegen-lexical-var-lookup (result depth-index env)
  "Generate IR for lookup of lexical value in runtime-env using depth and index"
  (let* ((depth (car depth-index))
	 (index (cadr depth-index))
	 (runtime-env (irc-renv env)))
    (dbg-set-current-debug-location-here)
    (cmp-log "About to call lexicalValueRead depth-index[%s] depth[%d] index[%d]\n" depth-index depth index)
    (irc-call env "lexicalValueRead" result (jit-constant-i32 depth) (jit-constant-i32 index) runtime-env))
  result)


(defun codegen-var-lookup (result sym old-env)
  "Return IR code thsym returns the value of a symbol that is either lexical or special"
  (cmp-log "About to codegen-var-lookup for %s\n" sym)
  (let ((classified (irc-classify-variable old-env sym)))
    (if (eq (car classified) 'core::special-var)
	(codegen-special-var-lookup result sym old-env)
	(let ((depth-index (cddr classified)))
	  (codegen-lexical-var-lookup result depth-index old-env)))))


(defun codegen-symbol (result at old-env)
  (if (keywordp at)
      (irc-call old-env "symbolValueRead" result (irc-global-symbol at))
      (codegen-var-lookup result at old-env)))

  


