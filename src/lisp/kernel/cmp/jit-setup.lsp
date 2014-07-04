
;;
;; Prepare the llvm system
;;

;;(in-package :cmp)
(eval-when (eval compile load)
  (select-package :cmp))



(defun generate-target-triple ()
  "Uses *features* to generate the target triple for the current machine
using features defined in corePackage.cc"
  (let ((tt nil))
    (setq tt
	  #+(and target-os-darwin address-model-64) "x86_64-apple-macosx10.7.0" ;; minimum required OSX level (Mountain Lion)
	  #+(and target-os-linux address-model-32) "i386-pc-linux-gnu"
	  #+(and target-os-linux address-model-64) "x86_64-pc-linux-gnu"
	  )
    (if tt
	tt
	(error "Could not calculate target triple"))))



(defvar *default-target-triple* (generate-target-triple)
  "The default target-triple for this machine")


(defun llvm-create-module (name)
  (let ((m (llvm-sys:make-module name *llvm-context*)))
    (llvm-sys:set-target-triple m *default-target-triple*)
    m))






(defun create-llvm-module-for-compile-file (module-name)
  "Return a new module and the function-pass-manager for the module"
  (let ((module (llvm-create-module module-name))
	fpm)
    ;; Define the primitives for the module
    (define-primitives-in-module module)
    (if *use-function-pass-manager-for-compile-file*
	(setq fpm (create-function-pass-manager-for-compile-file module)))
    (values module fpm)))



(defvar *use-function-pass-manager-for-compile-file* t)
(defun create-function-pass-manager-for-compile-file (module)
  (let ((fpm (llvm-sys:make-function-pass-manager module))
        (data-layout-pass (llvm-sys:make-data-layout-pass *data-layout*)) )
    (llvm-sys:function-pass-manager-add fpm data-layout-pass) ;; (llvm-sys:data-layout-copy *data-layout*))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-basic-alias-analysis-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-instruction-combining-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-reassociate-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-gvnpass nil))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-cfgsimplification-pass))
    (if *debug-ir*
      (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-debug-irpass "createDebugIR.log")))
    (llvm-sys:do-initialization fpm)
    fpm))



(defun create-function-pass-manager-for-compile (module)
  (let ((function-pass-manager (llvm-sys:make-function-pass-manager module))
        (data-layout-pass (llvm-sys:make-data-layout-pass *data-layout*)) )
    (llvm-sys:function-pass-manager-add function-pass-manager data-layout-pass)
    (llvm-sys:function-pass-manager-add function-pass-manager (llvm-sys:create-basic-alias-analysis-pass))
    (llvm-sys:function-pass-manager-add function-pass-manager (llvm-sys:create-instruction-combining-pass))
    (llvm-sys:function-pass-manager-add function-pass-manager (llvm-sys:create-reassociate-pass))
    (llvm-sys:function-pass-manager-add function-pass-manager (llvm-sys:create-gvnpass nil))
    (llvm-sys:function-pass-manager-add function-pass-manager (llvm-sys:create-cfgsimplification-pass))
    (llvm-sys:do-initialization function-pass-manager)
    function-pass-manager
    )
  )


(defvar *run-time-module-counter* 1)
(defun next-run-time-module-name ()
  "Return the next module name"
  (prog1
      (bformat nil "module%s" *run-time-module-counter*)
    (setq *run-time-module-counter* (+ 1 *run-time-module-counter*))))


(defun create-run-time-module-for-compile ()
  "Run time modules are used by COMPILE - a new one needs to be created for every COMPILE.
Return the module and the global variable that represents the load-time-value-holder as
\(value module global-run-time-values function-pass-manager\)."
  (let* ((module (llvm-create-module (next-run-time-module-name)))
	 (engine-builder (llvm-sys:make-engine-builder module)))
    (llvm-sys:set-target-options engine-builder '(llvm-sys:jitemit-debug-info t
						  llvm-sys:jitemit-debug-info-to-disk t))
    (llvm-sys:set-use-mcjit engine-builder t)
    (let* ((execution-engine (llvm-sys:create engine-builder))
	   (function-pass-manager (create-function-pass-manager-for-compile module)))
      (values module execution-engine function-pass-manager))))





(llvm-sys:initialize-native-target)
(defvar *llvm-context* (llvm-sys::get-global-context))

(defvar *run-time-module* nil)

(defvar *load-time-value-holder-name* "load-time-value-vector")

(defvar *the-module* nil
  "This stores the module into which compile puts its stuff")


(defvar *the-module-dibuilder* nil
  "Keeps track of the current DIBuilder for generating DWARF debugging information for *the-module*.
No DIBuilder is defined for the default module")

(defun jit-constant-pointer-null-get (type)
  (llvm-sys:constant-pointer-null-get type))


(defun jit-constant-i1 (val)
  "Create an i1 constant in the current context"
  (let ((ap-arg (llvm-sys:make-apint1 val)))
    (llvm-sys:constant-int-get *llvm-context* ap-arg)))

(defun jit-constant-i8 (val)
  "Create an i1 constant in the current context"
  (let ((ap-arg (llvm-sys:make-apint-width val 8 nil)))
    (llvm-sys:constant-int-get *llvm-context* ap-arg)))

(defun jit-constant-i32 (val)
  "Create a signed i32 constant in the current context"
  (let ((ap-arg (llvm-sys:make-apint-width val 32 t)))
    (llvm-sys:constant-int-get *llvm-context* ap-arg)))


(defun jit-constant-i32-vector-ptr (vals)
  (let* ((constant-data-array (llvm-sys:constant-data-array-get-uint32 *llvm-context* vals))
	 (constant-data-array-type (llvm-sys:get-type constant-data-array))
	 (global-var-for-constant-array (llvm-sys:make-global-variable *the-module*
								       constant-data-array-type
								       t
								       'llvm-sys:internal-linkage
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
;;    (llvm-sys:create-in-bounds-gep *irbuilder* unique-string-global-variable (list (jit-constant-i32 0) (jit-constant-i32 0)) label )
    (llvm-sys:constant-expr-get-in-bounds-get-element-ptr unique-string-global-variable (list (jit-constant-i32 0) (jit-constant-i32 0)))
    )
)




(defun fasl-pathname-type-impl (file-type &key min ref-count)
  (core:bformat nil "%s%s%s"
                file-type
                (if min "-min" "")
                (if ref-count "-rc" "")))

(defun search* (sym list)
  (if (null list)
      nil
      (if (eq (car list) sym)
          t
          (search* sym (cdr list)))))

(defun fasl-pathname-type (file-type)
  (let ((is-min (cmp::search* :ecl-min *features*))
        (is-ref-count (cmp::search* :use-refcount *features*)))
    (fasl-pathname-type-impl file-type
                             :min is-min
                             :ref-count is-ref-count)))
(export 'fasl-pathname-type)


(defun jit-function-name (lname)
  "Depending on the type of LNAME an actual LLVM name is generated"
  (cond
    ((pathnamep lname) (bformat nil "__MAIN_%s" (pathname-name lname)))
    ((symbolp lname) (bformat nil "FN-SYMB.%s" (symbol-name lname)))
    ((stringp lname) lname)
    ((and (consp lname) (eq (car lname) 'setf)) (bformat nil "FN-SETF.%s" (symbol-name (cadr lname))))
    (t (error "Illegal lisp function name[~a]" lname))))
(export 'jit-function-name)

(setq core:*llvm-function-name-hook* #'jit-function-name)


(si::*fset
 'load-bitcode
 #'(lambda (filename)
     "Load a bitcode file, link it and execute it"
     (let ((*package* *package*)
	   (time-load-start (clock-gettime-nanoseconds)))
       (bformat t "Loading module from file: %s\n" filename)
       (let* ((mod (llvm-sys:parse-bitcode-file (namestring (truename filename)) *llvm-context*))
	      (engine-builder (llvm-sys:make-engine-builder mod)))
	 ;; TODO: Set this up to use MCJIT
	 (llvm-sys:set-target-options engine-builder
				      '(llvm-sys:jitemit-debug-info t
					llvm-sys:jitemit-debug-info-to-disk t))
	 (llvm-sys:set-use-mcjit engine-builder t)
	 (let*((execution-engine (llvm-sys:create engine-builder))
	       (stem (string-downcase (pathname-name filename)))
               (main-fn-name (jit-function-name (pathname filename)))
	       (main-fn (llvm-sys:get-function mod main-fn-name))
	       (time-jit-start (clock-gettime-nanoseconds)))
           (if (not main-fn)
             (error "Could not find main function ~a loaded from file ~a" main-fn-name filename))
	   (llvm-sys:finalize-engine-and-register-with-gc-and-run-function execution-engine main-fn (namestring (truename filename)) *load-time-value-holder-name* )
	   ))))
 nil)
(export 'load-bitcode)


