
;;
;; Prepare the llvm system
;;

;;(in-package :cmp)
(eval-when (eval compile load)
  (select-package :cmp))


(llvm-sys:initialize-native-target)
(defparameter *llvm-context* (llvm-sys::get-global-context))
(defparameter *the-module* (llvm-sys:make-module "main-module" *llvm-context*))
(if (is-undefined *the-module*)
    (error "The LLVM module could not be created"))


(defun jit-constant-pointer-null-get (type)
  (llvm-sys:constant-pointer-null-get type))


(defun jit-constant-i1 (val)
  "Create an i1 constant in the current context"
  (let ((ap-arg (llvm-sys:make-apint1 val)))
    (llvm-sys:constant-int-get *llvm-context* ap-arg)))

(defun jit-constant-i32 (val)
  "Create an i32 constant in the current context"
  (let ((ap-arg (llvm-sys:make-apint val)))
    (llvm-sys:constant-int-get *llvm-context* ap-arg)))


(defun jit-constant-i32-vector-ptr (vals)
  (let* ((constant-data-array (llvm-sys:constant-data-array-get-uint32 *llvm-context* vals))
	 (constant-data-array-type (llvm-sys:get-type constant-data-array))
	 (global-var-for-constant-array (llvm-sys:make-global-variable *the-module*
								       constant-data-array-type
								       t
								       'llvm-sys:private-linkage
								       constant-data-array
								       "constant-array"))
	 (gep (llvm-sys:constant-expr-get-in-bounds-get-element-ptr global-var-for-constant-array
								    (list (jit-constant-i32 0) (jit-constant-i32 0)))))
    gep))







(defun jit-constant-i64 (val)
  "Create an i1 constant in the current context"
  (let ((ap-arg (llvm-sys:make-apint64 val)))
    (llvm-sys:constant-int-get *llvm-context* ap-arg)))


(defun jit-constant-unique-string-ptr (sn &optional (label "unique-str"))
  "Get or create a unique string within the module and return a GEP i8* pointer to it"
  (let* ((sn-gv (llvm-sys:get-or-create-uniqued-string-global-variable
		 *the-module* sn
		 (bformat nil ":::symbol-name-%s" sn)))
	 (sn-value-ptr (llvm-sys:create-in-bounds-gep
			*irbuilder* sn-gv (list (jit-constant-i32 0) (jit-constant-i32 0)) "sn")))
    sn-value-ptr))


(defun jit-make-global-string-ptr (str &optional (label "global-str"))
  "A function for creating unique strings within the module - return an LLVM pointer to the string"
  (let ((unique-string-global-variable (llvm-sys:get-or-create-uniqued-string-global-variable *the-module* str (bformat nil ":::global-str-%s" str))))
    (llvm-sys:create-in-bounds-gep *irbuilder* unique-string-global-variable (list (jit-constant-i32 0) (jit-constant-i32 0)) label ))
)













(defparameter *engine-builder* (llvm-sys:make-engine-builder *the-module*))

(llvm-sys:set-target-options *engine-builder* '( :jitemit-debug-info t
						:jitemit-debug-info-to-disk t
						:jitexception-handling t))
(defparameter *the-execution-engine* (llvm-sys:create *engine-builder*))
(if (is-undefined *the-execution-engine*)
  (error "The execution engine could not be created: ~a" (llvm-sys:error-string *engine-builder*)))

(export '(*llvm-context* *the-module* *engine-builder* *the-execution-engine*))





(defparameter *the-function-pass-manager* (llvm-sys:make-function-pass-manager *the-module*))

;; The function-pass-manager for the default *the-module*
(llvm-sys:function-pass-manager-add *the-function-pass-manager* (llvm-sys:data-layout-copy (llvm-sys:get-data-layout *the-execution-engine*)))
(llvm-sys:function-pass-manager-add *the-function-pass-manager* (llvm-sys:create-basic-alias-analysis-pass))
(llvm-sys:function-pass-manager-add *the-function-pass-manager* (llvm-sys:create-instruction-combining-pass))
(llvm-sys:function-pass-manager-add *the-function-pass-manager* (llvm-sys:create-reassociate-pass))
(llvm-sys:function-pass-manager-add *the-function-pass-manager* (llvm-sys:create-gvnpass nil))
(llvm-sys:function-pass-manager-add *the-function-pass-manager* (llvm-sys:create-cfgsimplification-pass))
(llvm-sys:do-initialization *the-function-pass-manager*)




(defun jit-function-name (lisp-function-name)
  (if (symbolp lisp-function-name)
      (return-from jit-function-name (bformat nil "FN-SYMB.%s" (symbol-name lisp-function-name)))
      (if (stringp lisp-function-name)
	  (return-from jit-function-name lisp-function-name)
	  (if (consp lisp-function-name)
	      (if (eq (car lisp-function-name) 'core:setf)
		  (return-from jit-function-name (bformat nil "FN-SETF.%s" (symbol-name (cadr lisp-function-name))))
		  nil))))
  (error "Illegal lisp function name[~a]" lisp-function-name))



(si::*fset 'bcload
	   #'(lambda (filename)
	       "Load a bitcode file, link it and execute it"
	       (let ((save-package *package*))
		 ;;		(bformat t "Loading module from file: %s\n" filename)
		 (let ((mod (llvm-sys:parse-bitcode-file filename *llvm-context* *the-execution-engine*)))
		   ;;		  (bformat t "loaded module is valid = %d\n" (llvm-sys:module-valid mod))
		   (llvm-sys:link-external-globals-in-module mod *the-execution-engine*)
		   (let* ((stem (path-stem (make-path filename)))
			  (main-fn (llvm-sys:get-function mod (bformat nil "___main_%s" stem))))
		     (llvm-sys:run-function *the-execution-engine* main-fn nil)))
		 (setq *package* save-package)))
	   nil)
(export 'bcload)
