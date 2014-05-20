
(defparameter *engine-builder* (llvm-sys:make-engine-builder *the-module*))
;;
;; --------       Here set the execution engine kind 
;;                INTERPRETER or JIT (for native code)
;;
;;(llvm-sys:set-engine-kind *engine-builder* 'llvm-sys:interpreter)
(llvm-sys:set-target-options *engine-builder* '(llvm-sys:jitemit-debug-info t))
(defparameter *the-execution-engine* (llvm-sys:create *engine-builder*))
(if (is-undefined *the-execution-engine*)
  (error "The execution engine could not be created: ~a" (llvm-sys:error-string *engine-builder*)))
