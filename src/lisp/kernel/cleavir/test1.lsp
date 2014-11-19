(load "tools.lsp")

(build-and-draw-ast "test.dot" *code3*)

(defparameter *code4* '(defun codegen (result form env)
			(declare (optimize (debug 3)))
			(assert-result-isa-llvm-value result)
			(multiple-value-bind (source-directory source-filename lineno column)
			    (dbg-set-current-source-pos env form)
			  (let* ((*current-form* form)
				 (*current-env* env)
				 (*current-lineno* (if lineno lineno *current-lineno*))
				 (*current-column* (if column column *current-column*)))
			    (cmp-log "codegen stack-used[%d bytes]\n" (stack-used))
			    (cmp-log "codegen evaluate-depth[%d]  %s\n" (evaluate-depth) form)
			    ;;
			    ;; If a *code-walker* is defined then invoke the code-walker
			    ;; with the current form and environment
			    (when *code-walker*
			      (setq form (funcall *code-walker* form env)))
			    (if (atom form)
				(codegen-atom result form env)
				(let ((head (car form))
				      (rest (cdr form)))
				  (cmp-log "About to codegen special-operator or application for: %s\n" form)
				  ;;	(trace-linenumber-column (walk-to-find-parse-pos form) env)
				  (if (and head (symbolp head) (augmented-special-operator-p head))
				      (progn
					(cmp-log "About to codegen-special-operator: %s %s\n" head rest)
					(codegen-special-operator result head rest env))
				      (progn
					(cmp-log "About to codegen-application: %s\n" form)
					(codegen-application result form env))))))))
  )

(build-and-draw-ast "big.dot" *code4*)
