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
  (core:select-package :cmp))


(defconstant +debug-dwarf-version+ 4)


(defun generate-target-triple ()
  "Uses *features* to generate the target triple for the current machine
using features defined in corePackage.cc"
  (cond
    ((and (member :target-os-darwin *features*)
          (member :address-model-64 *features*))
     (values "x86_64-apple-macosx10.10.0" "e-m:o-i64:64-f80:128-n8:16:32:64-S128"))
    ((and (member :target-os-linux *features*)
          (member :address-model-64 *features*))
     (values "x86_64-pc-linux-gnu"       "e-m:e-i64:64-f80:128-n8:16:32:64-S128"))
    (t (error "Could not identify target triple for features ~a" *features*))))

(defun generate-data-layout ()
  (let* ((triple (generate-target-triple))
         (target (llvm-sys:target-registry-lookup-target.string triple))
         (target-options (llvm-sys:make-target-options))
         (target-machine (llvm-sys:create-target-machine
                          target
                          triple
                          ""
                          ""
                          target-options
                          'llvm-sys:reloc-model-undefined
                          'llvm-sys:code-model-default
                          'llvm-sys:code-gen-opt-default)))
    (llvm-sys:create-data-layout target-machine)))

(defvar *default-target-triple* (generate-target-triple)
  "The default target-triple for this machine")

(defvar *default-data-layout* (generate-data-layout))

(defun llvm-create-module (name)
  (let ((m (llvm-sys:make-module (string name) *llvm-context*)))
    (llvm-sys:set-target-triple m *default-target-triple*)
    (llvm-sys:set-data-layout m *default-data-layout*)
    m))

(defun create-llvm-module-for-compile-file (module-name)
  "Return a new module"
  (let ((module (llvm-create-module module-name)))
    ;; Define the primitives for the module
    (define-primitives-in-module module)
    module))



#+(or)(defvar *use-function-pass-manager-for-compile-file* t)
#+(or)
(defun create-function-pass-manager-for-compile-file (module)
  (let ((fpm (llvm-sys:make-function-pass-manager module)))
    (warn "Look at what passes the Kaleidoscope demo is making")
    ;;    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-basic-alias-analysis-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-instruction-combining-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-promote-memory-to-register-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-reassociate-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-gvnpass nil))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-cfgsimplification-pass -1 #'(lambda (f) t)))
    ;;    (if *debug-ir* (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-debug-irpass "createDebugIR.log")))
    (llvm-sys:do-initialization fpm)
    fpm))


#+(or)
(defun create-function-pass-manager-for-compile (module)
  (let ((fpm (llvm-sys:make-function-pass-manager module)))
    (warn "Look at what passes the Kaleidoscope demo is making")
    ;;    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-basic-alias-analysis-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-instruction-combining-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-promote-memory-to-register-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-reassociate-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-gvnpass nil))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-cfgsimplification-pass -1 #'(lambda (f) t)))
    (llvm-sys:do-initialization fpm)
    fpm))


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
    (llvm-sys:set-target-options engine-builder target-options)
    (llvm-sys:create engine-builder)))




(llvm-sys:initialize-native-target)
;;;(defvar *llvm-context* (llvm-sys::get-global-context))

(defvar *run-time-module* nil)

;;;(defvar *load-time-value-holder-name* "load-time-value-vector")

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
	 (gep (llvm-sys:constant-expr-get-in-bounds-get-element-ptr
               nil
               global-var-for-constant-array
               (list (jit-constant-i32 0) (jit-constant-i32 0)))))
    gep))







(defun jit-constant-i64 (val)
  "Create an i1 constant in the current context"
  (let ((ap-arg (llvm-sys:make-apint64 val)))
    (llvm-sys:constant-int-get *llvm-context* ap-arg)))


(defun jit-constant-size_t (val)
  (let ((sizeof-size_t (cdr (assoc 'core:size-t (llvm-sys:cxx-data-structures-info)))))
    (cond
      ((= 8 sizeof-size_t) (jit-constant-i64 val))
      ((= 4 sizeof_size_t) (jit-constant-i32 val))
      (t (error "Add support for size_t sizeof = ~a" sizeof-size_t)))))

(defun jit-constant-size_t (val)
  (let ((sizeof-size_t (cdr (assoc 'core:size-t (llvm-sys:cxx-data-structures-info)))))
    (cond
      ((= 8 sizeof-size_t) (jit-constant-i64 val))
      ((= 4 sizeof_size_t) (jit-constant-i32 val))
      (t (error "Add support for size_t sizeof = ~a" sizeof-size_t)))))


(defun jit-constant-unique-string-ptr (sn &optional (label "unique-str"))
  "Get or create a unique string within the module and return a GEP i8* pointer to it"
  (or *the-module* (error "jit-constant-unique-string-ptr *the-module* is NIL"))
  (let* ((sn-gv (llvm-sys:get-or-create-uniqued-string-global-variable
		 *the-module* sn
		 (bformat nil "str-%s" sn)))
	 (sn-value-ptr (llvm-sys:create-in-bounds-gep
			*irbuilder* sn-gv (list (jit-constant-i32 0) (jit-constant-i32 0)) "sn")))
    sn-value-ptr))


(defun jit-make-global-string-ptr (str &optional (label "global-str"))
  "A function for creating unique strings within the module - return an LLVM pointer to the string"
  (or *the-module* (error "jit-make-global-string-ptr *the-module* is NIL"))
  (let ((unique-string-global-variable (llvm-sys:get-or-create-uniqued-string-global-variable *the-module* str (bformat nil ":::global-str-%s" str))))
;;    (llvm-sys:create-in-bounds-gep *irbuilder* unique-string-global-variable (list (jit-constant-i32 0) (jit-constant-i32 0)) label )
    (llvm-sys:constant-expr-get-in-bounds-get-element-ptr
     nil
     unique-string-global-variable
     (list (jit-constant-i32 0) (jit-constant-i32 0)))
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
    ((stringp lname)
     (cond
       ((string= lname core:+run-all-function-name+) lname) ; this one is ok
       ((string= lname core:+clasp-ctor-function-name+) lname) ; this one is ok
       ((string= lname "IMPLICIT-REPL") lname) ; this one is ok
       ((string= lname "TOP-LEVEL") lname) ; this one is ok
       ((string= lname "UNNAMED-LAMBDA") lname) ; this one is ok
       ((string= lname "lambda") lname) ; this one is ok
       ((string= lname "ltv-literal") lname) ; this one is ok
       (t (bformat t "jit-function-name lname = %s\n" lname)
          (break "Always pass symbol as name") lname)))
    ((symbolp lname)
     (let* ((sym-pkg (symbol-package lname))
            (sym-name (symbol-name lname))
            (pkg-name (if sym-pkg
                          (string (package-name sym-pkg))
                          "NIL")))
       (bformat nil "FN/%s::%s" pkg-name sym-name)))
    ((and (consp lname) (eq (car lname) 'setf))
     (let* ((sn (cadr lname))
            (sym-pkg (symbol-package sn))
            (sym-name (symbol-name sn))
            (pkg-name (if sym-pkg
                          (string (package-name sym-pkg))
                          "NIL")))
       (bformat nil "FN-SETF/%s::%s" pkg-name sym-name)))
    (t (error "Illegal lisp function name[~a]" lname))))
(export 'jit-function-name)

(setq core:*llvm-function-name-hook* #'jit-function-name)

;;; Use the one in llvm-sys
#+(or)
(defun load-bitcode (filename &optional verbose print external_format)
  "Load a bitcode file, link it and execute it"
  (let* ((*package* *package*)
         (module (llvm-sys:parse-bitcode-file (namestring (truename filename)) *llvm-context*))
         (engine-builder (llvm-sys:make-engine-builder module))
         (target-options (llvm-sys:make-target-options)))
    (llvm-sys:set-target-options engine-builder target-options)
;;;	 (llvm-sys:set-use-mcjit engine-builder t)
    (let* ((execution-engine (llvm-sys:create engine-builder)))
      (llvm-sys:finalize-engine-and-register-with-gc-and-run-main-functions execution-engine 
                                                                            *load-time-value-holder-name*
                                                                            (namestring (truename filename)))
      t)))
(export 'load-bitcode)
