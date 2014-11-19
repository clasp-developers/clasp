;;;
;;;    File: jit-setup.lsp
;;;

;; Copyright (c) 2014, Christian E. Schafmeister
;;
;; CLASP is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Library General Public
;; License as published by the Free Software Foundation; either
;; version 2 of the License, or (at your option) any later version.
;;
;; See directory 'clasp/licenses' for full details.
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;; -^-

;;
;; Prepare the llvm system
;;

;;(in-package :cmp)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (select-package :cmp))


(defconstant +debug-dwarf-version+ 4)


(defun generate-target-triple ()
  "Uses *features* to generate the target triple for the current machine
using features defined in corePackage.cc"
  (let ((tt nil))
    (setq tt
	  #+(and target-os-darwin address-model-64) "x86_64-apple-macosx10.9.4" ;; minimum required OSX level (Mountain Lion)
	  #+(and target-os-linux address-model-32) "i386-pc-linux-gnu"
	  #+(and target-os-linux address-model-64) "x86_64-unknown-linux-gnu"
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


(defun make-gv-source-file-info-handle-in-*the-module* ()
  (llvm-sys:make-global-variable *the-module*
                                 +i32+  ; type
                                 nil    ; constant
                                 'llvm-sys:internal-linkage
                                 (jit-constant-i32 -1)
                                 "source-file-info-handle"))


(defun make-boot-function-global-variable (module func-ptr)
  (llvm-sys:make-global-variable module
                                 +fn-void-ptr-array1+ ; type
                                 t ; is constant
                                 'llvm-sys:appending-linkage
                                 (llvm-sys:constant-array-get +fn-void-ptr-array1+ (list func-ptr))
                                 llvm-sys:+global-boot-functions-name+)
  (llvm-sys:make-global-variable module
                                 +i32+ ; type
                                 t ; is constant
                                 'llvm-sys:internal-linkage
                                 (jit-constant-i32 1)
                                 llvm-sys:+global-boot-functions-name-size+)
  )




(defun remove-main-function-if-exists (module)
  (let ((fn (llvm-sys:get-function module llvm-sys:+clasp-main-function-name+)))
    (if fn
      (llvm-sys:erase-from-parent fn))))

(defun reset-global-boot-functions-name-size (module)
  (remove-main-function-if-exists module)
  (let* ((funcs (llvm-sys:get-named-global module llvm-sys:+global-boot-functions-name+))
         (ptype (llvm-sys:get-type funcs))
         (atype (llvm-sys:get-sequential-element-type ptype))
         (num-elements (llvm-sys:get-array-num-elements atype))
         (var (llvm-sys:get-global-variable module llvm-sys:+global-boot-functions-name-size+ t)))
    (if var (llvm-sys:erase-from-parent var))
    (llvm-sys:make-global-variable module
                                   +i32+ ; type
                                   t     ; is constant
                                   'llvm-sys:internal-linkage
                                   (jit-constant-i32 num-elements)
                                   llvm-sys:+global-boot-functions-name-size+)))

(defun add-main-function (module)
  (let ((*the-module* module))
    (remove-main-function-if-exists module)
    (let ((fn (with-new-function
                  (main-func func-env
                             :function-name llvm-sys:+clasp-main-function-name+
                             :parent-env nil
                             :linkage 'llvm-sys:external-linkage
                             :function-type +fn-void+
                             :argument-names nil)
                (let* ((boot-functions (llvm-sys:get-global-variable module llvm-sys:+global-boot-functions-name+ t))
                       (boot-functions-size (llvm-sys:get-global-variable module llvm-sys:+global-boot-functions-name-size+ t))
                       (bc-bf (llvm-sys:create-bit-cast *irbuilder* boot-functions +fn-void-ptr-pointer+ "fnptr-pointer"))
                       )
                  (irc-intrinsic "invokeMainFunctions" bc-bf boot-functions-size)))))
      fn)))





(defun create-llvm-module-for-compile-file (module-name)
  "Return a new module"
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
        ;;        (data-layout-pass (llvm-sys:make-data-layout-pass *data-layout*))
        )
;;    (llvm-sys:function-pass-manager-add fpm data-layout-pass) ;; (llvm-sys:data-layout-copy *data-layout*))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-basic-alias-analysis-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-instruction-combining-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-promote-memory-to-register-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-reassociate-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-gvnpass nil))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-cfgsimplification-pass))
;;    (if *debug-ir* (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-debug-irpass "createDebugIR.log")))
    (llvm-sys:do-initialization fpm)
    fpm))


(defun create-function-pass-manager-for-compile (module)
  (let ((fpm (llvm-sys:make-function-pass-manager module))
;;        (data-layout-pass (llvm-sys:make-data-layout-pass *data-layout*))
        )
;;    (llvm-sys:function-pass-manager-add fpm data-layout-pass)
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-basic-alias-analysis-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-instruction-combining-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-promote-memory-to-register-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-reassociate-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-gvnpass nil))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-cfgsimplification-pass))
    (llvm-sys:do-initialization fpm)
    fpm
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
  (llvm-create-module (next-run-time-module-name)))

(defun create-run-time-execution-engine (module)
  (let ((engine-builder (llvm-sys:make-engine-builder module))
	(target-options (llvm-sys:make-target-options)))
    (llvm-sys:setf-jitemit-debug-info target-options t)
    (llvm-sys:setf-jitemit-debug-info-to-disk target-options t)
    (llvm-sys:set-target-options engine-builder target-options)
    (llvm-sys:create engine-builder)))




(llvm-sys:initialize-native-target)
(defvar *llvm-context* (llvm-sys::get-global-context))

(defvar *run-time-module* nil)

(defvar *load-time-value-holder-name* "load-time-value-vector")

(defvar *the-module* nil "This stores the module into which compile puts its stuff")
(defvar *run-time-execution-engine* nil)
(defvar *the-function-pass-manager* nil "the function-pass-manager applied to runtime functions")


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





(defun search* (sym list)
  (if (null list)
      nil
      (if (eq (car list) sym)
          t
          (search* sym (cdr list)))))

(defparameter *compile-file-target* nil)
(defconstant +target-min-boehm+ :min-boehm)
(defconstant +target-full-boehm+ :full-boehm)
(defconstant +target-min-mps+ :min-mps)
(defconstant +target-full-mps+ :full-mps)


(defun jit-function-name (lname)
  "Depending on the type of LNAME an actual LLVM name is generated"
  (cond
    ((pathnamep lname) (bformat nil "MAIN-%s" (string-upcase (pathname-name lname))))
    ((symbolp lname) (bformat nil "FN-SYMB.%s" (symbol-name lname)))
    ((stringp lname) lname)
    ((and (consp lname) (eq (car lname) 'setf)) (bformat nil "FN-SETF.%s" (symbol-name (cadr lname))))
    (t (error "Illegal lisp function name[~a]" lname))))
(export 'jit-function-name)

(setq core:*llvm-function-name-hook* #'jit-function-name)


(si::*fset
 'load-bitcode
 #'(lambda (filename &optional verbose print external_format)
     "Load a bitcode file, link it and execute it"
     (let ((*package* *package*)
	   (time-load-start (clock-gettime-nanoseconds)))
;;       (bformat t "Loading module from file: %s\n" filename)
       (let* ((module (llvm-sys:parse-bitcode-file (namestring (truename filename)) *llvm-context*))
	      (engine-builder (llvm-sys:make-engine-builder module))
	 ;; After make-engine-builder MODULE becomes invalid!!!!!
	      (target-options (llvm-sys:make-target-options)))
	 (llvm-sys:setf-jitemit-debug-info target-options t)
	 (llvm-sys:setf-jitemit-debug-info-to-disk target-options t)
	 (llvm-sys:set-target-options engine-builder target-options)
;;;	 (llvm-sys:set-use-mcjit engine-builder t)
	 (let*((execution-engine (llvm-sys:create engine-builder))
	       (stem (string-downcase (pathname-name filename)))
               (main-fn-name llvm-sys:+clasp-main-function-name+)
	       (time-jit-start (clock-gettime-nanoseconds)))
	   (llvm-sys:finalize-engine-and-register-with-gc-and-run-function execution-engine main-fn-name (namestring (truename filename)) 0 *load-time-value-holder-name* )
	   )))
     t)
 nil)
(export 'load-bitcode)
