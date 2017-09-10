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


;;;
;;; Until we have proper source tracking set this to the current source line number
;;; whenever we read a new form in compile-file or evaluate the DEFUN macro.
;;; This should give us at least some level of source location information
;;;
(defvar *current-form-lineno* 0)

(defconstant +debug-dwarf-version+ 4)

;; bound when a new thread is created
(defvar *primitives*)
(export '*primitives*)

;;;(defvar *llvm-context* (llvm-sys:create-llvm-context))
(mp:push-default-special-binding 'cmp:*llvm-context* '(llvm-sys:create-llvm-context))
(mp:push-default-special-binding 'cmp:*primitives* nil)


(defun dump-function (func)
  (warn "Do something with dump-function"))
(export 'dump-function)

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
    (if (member :use-orc *features*)
        (progn
          (llvm-sys:set-use-orc-mcjitreplacement engine-builder t)
          (bformat t "Using ORC\n")))
    (llvm-sys:create engine-builder)))




(llvm-sys:initialize-native-target)

(defvar *run-time-module* nil)

(defvar *the-module* nil "This stores the module into which compile puts its stuff")
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
  "Create an i64 constant in the current context"
  (let ((ap-arg (llvm-sys:make-apint-width val 64 t)))
    (llvm-sys:constant-int-get *llvm-context* ap-arg)))

(defun jit-constant-ui32 (val)
  "Create an unsigned i32 constant in the current context"
  (let ((ap-arg (llvm-sys:make-apint-width val 32 nil)))
    (llvm-sys:constant-int-get *llvm-context* ap-arg)))

(defun jit-constant-ui64 (val)
  "Create an unsigned i64 constant in the current context"
  (let ((ap-arg (llvm-sys:make-apint-width val 64 nil)))
    (llvm-sys:constant-int-get *llvm-context* ap-arg)))


(defun jit-constant-size_t (val)
  (let ((sizeof-size_t (cdr (assoc 'core:size-t (llvm-sys:cxx-data-structures-info)))))
    (cond
      ((= 8 sizeof-size_t) (jit-constant-i64 val))
      ((= 4 sizeof-size_t) (jit-constant-i32 val))
      (t (error "Add support for size_t sizeof = ~a" sizeof-size_t)))))

(defun jit-constant-uintptr_t (val)
  (let ((sizeof-size_t (cdr (assoc 'core:size-t (llvm-sys:cxx-data-structures-info)))))
    (cond
      ((= 8 sizeof-size_t) (jit-constant-ui64 val))
      ((= 4 sizeof-size_t) (jit-constant-ui32 val))
      (t (error "Add support for size_t sizeof = ~a" sizeof-size_t)))))

(defun jit-constant-intptr_t (val)
  (let ((sizeof-size_t (cdr (assoc 'core:size-t (llvm-sys:cxx-data-structures-info)))))
    (cond
      ((= 8 sizeof-size_t) (jit-constant-i64 val))
      ((= 4 sizeof-size_t) (jit-constant-i32 val))
      (t (error "Add support for size_t sizeof = ~a" sizeof-size_t)))))

(defun jit-constant-size_t (val)
  (let ((sizeof-size_t (cdr (assoc 'core:size-t (llvm-sys:cxx-data-structures-info)))))
    (cond
      ((= 8 sizeof-size_t) (jit-constant-ui64 val))
      ((= 4 sizeof-size_t) (jit-constant-ui32 val))
      (t (error "Add support for size_t sizeof = ~a" sizeof-size_t)))))


(defun jit-constant-unique-string-ptr (str &optional (label "unique-str"))
  "Get or create a unique string within the module and return a GEP i8* pointer to it"
  (or *the-module* (error "jit-constant-unique-string-ptr *the-module* is NIL"))
  (let* ((str-gv (llvm-sys:get-or-create-uniqued-string-global-variable
                 *the-module* str
                 (bformat nil "str-%s" str))))
    (llvm-sys:create-const-gep2-64 *irbuilder* str-gv 0 0 "str")))


(defun module-make-global-string (str &optional (label "global-str"))
  "A function for creating unique strings within the module - return an LLVM pointer to the string"
  (or *the-module* (error "module-make-global-string-ptr *the-module* is NIL"))
  (let* ((unique-string-global-variable
          (llvm-sys:get-or-create-uniqued-string-global-variable
           *the-module* str (bformat nil ":::global-str-%s" str)))
         (string-type (llvm-sys:array-type-get (llvm-sys:type-get-int8-ty *llvm-context*) (1+ (length str)))))
    unique-string-global-variable))
#|
    (let ((gep (llvm-sys:constant-expr-get-in-bounds-get-element-ptr
                string-type
                unique-string-global-variable
                (list (jit-constant-i32 0) (jit-constant-i32 0)))))
      gep)))
|#


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
;;  (break "Check backtrace")
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
       (bformat nil "%s^^%s_FN" sym-name pkg-name)))
    ((and (consp lname) (eq (car lname) 'setf))
     (let* ((sn (cadr lname))
            (sym-pkg (symbol-package sn))
            (sym-name (symbol-name sn))
            (pkg-name (if sym-pkg
                          (string (package-name sym-pkg))
                          "NIL")))
       (bformat nil "%s^^%s_SETF" sym-name pkg-name)))
    ((and (consp lname) (eq (car lname) 'method) (symbolp (second lname)))
     (let ((pkg-name (string (package-name (symbol-package (second lname)))))
           (name (symbol-name (second lname))))
     (bformat nil "%s^^%s(METHOD %s)" name pkg-name (cddr lname))))
    ((consp lname)
     (bformat nil "%s_FN" lname))
    (t (error "Illegal lisp function name[~a]" lname))))
(export 'jit-function-name)

(setq core:*llvm-function-name-hook* #'jit-function-name)


;;; ------------------------------------------------------------
;;;
;;;  JIT facility
;;;



(defun jit-link-builtins-module (module)
  "Merge the intrinsics module with the passed module.
The passed module is modified as a side-effect."
  (unless *builtins-module*
    (let* ((builtins-bitcode-name (namestring (truename (build-inline-bitcode-pathname :compile :builtins))))
           (builtins-module (llvm-sys:parse-bitcode-file builtins-bitcode-name *llvm-context*)))
      (llvm-sys:remove-useless-global-ctors builtins-module)
      (setf *builtins-module* builtins-module)))
  ;; Clone the intrinsics module and link it in
  (quick-module-dump module "module before linking builtins-clone")
  (let ((linker (llvm-sys:make-linker module))
        (builtins-clone (llvm-sys:clone-module *builtins-module*)))
    ;;(remove-always-inline-from-functions builtins-clone)
    (quick-module-dump builtins-clone "builtins-clone")
    (llvm-sys:link-in-module linker builtins-clone))
  module)

(defun jit-link-fastgf-module (module)
  "Merge the intrinsics module with the passed module.
The passed module is modified as a side-effect."
  (unless *fastgf-module*
    (let* ((fastgf-bitcode-name (namestring (truename (build-inline-bitcode-pathname :compile :fastgf))))
           (fastgf-module (llvm-sys:parse-bitcode-file fastgf-bitcode-name *llvm-context*)))
      (llvm-sys:remove-useless-global-ctors fastgf-module)
      (setf *fastgf-module* fastgf-module)))
  ;; Clone the intrinsics module and link it in
  (quick-module-dump module "module before linking fastgf-clone")
  (let ((linker (llvm-sys:make-linker module))
        (fastgf-clone (llvm-sys:clone-module *fastgf-module*)))
    ;;(remove-always-inline-from-functions fastgf-clone)
    (quick-module-dump fastgf-clone "fastgf-clone")
    (llvm-sys:link-in-module linker fastgf-clone))
  module)

(defvar *optimizations-on* t)
(defvar *optimization-level* :-O3)
(defvar *size-level* 1)


(defun optimize-module-for-compile-file (module &optional (optimize-level *optimization-level*) (size-level *size-level*))
  (declare (type (or null llvm-sys:module) module))
  (when (and *optimizations-on* module)
    #++(let ((call-sites (call-sites-to-always-inline module)))
      (bformat t "Call-sites -> %s\n" call-sites))
    (let* ((pass-manager-builder (llvm-sys:make-pass-manager-builder))
           (mpm (llvm-sys:make-pass-manager))
           (fpm (llvm-sys:make-function-pass-manager module))
           (olevel (cond
                     ((eq optimize-level :-O3) 3)
                     ((eq optimize-level :-O2) 2)
                     ((eq optimize-level :-O1) 1)
                     ((eq optimize-level :-O0) 0)
                     (t (error "Unsupported optimize-level ~a - only :-O3 :-O2 :-O1 :-O0 are allowed" optimize-level)))))
      (llvm-sys:pass-manager-builder-setf-opt-level pass-manager-builder olevel)
      (llvm-sys:pass-manager-builder-setf-size-level pass-manager-builder size-level)
      (llvm-sys:populate-ltopass-manager pass-manager-builder mpm)
      (llvm-sys:pass-manager-run mpm module)))
  module)


(defun optimize-module-for-compile (module &optional (optimize-level *optimization-level*) (size-level *size-level*))
  (declare (type (or null llvm-sys:module) module))
  (when (and *optimizations-on* module)
    #++(let ((call-sites (call-sites-to-always-inline module)))
      (bformat t "Call-sites -> %s\n" call-sites))
    (let* ((pass-manager-builder (llvm-sys:make-pass-manager-builder))
           (mpm (llvm-sys:make-pass-manager))
           (fpm (llvm-sys:make-function-pass-manager module))
           (olevel (cond
                     ((eq optimize-level :-O3) 3)
                     ((eq optimize-level :-O2) 2)
                     ((eq optimize-level :-O1) 1)
                     ((eq optimize-level :-O0) 0)
                     (t (error "Unsupported optimize-level ~a - only :-O3 :-O2 :-O1 :-O0 are allowed" optimize-level)))))
      (llvm-sys:pass-manager-builder-setf-opt-level pass-manager-builder olevel)
      (llvm-sys:pass-manager-builder-setf-size-level pass-manager-builder size-level)
      (llvm-sys:pass-manager-builder-setf-inliner pass-manager-builder (llvm-sys:create-always-inliner-legacy-pass))
      (llvm-sys:populate-function-pass-manager pass-manager-builder fpm)
      (llvm-sys:populate-module-pass-manager pass-manager-builder mpm)
      ;; (llvm-sys:populate-ltopass-manager pass-manager-builder mpm)
      (llvm-sys:do-initialization fpm)
      (let ((funcs (llvm-sys:module-get-function-list module)))
        (dolist (func funcs)
          (llvm-sys:function-pass-manager-run fpm func)))
      (llvm-sys:do-finalization fpm)
      (llvm-sys:pass-manager-run mpm module)))
  module)



(defun remove-always-inline-from-functions (module)
  (let ((functions (llvm-sys:module-get-function-list module))
        inline-functions)
    (dolist (f functions)
      (if (llvm-sys:has-fn-attribute f 'llvm-sys:attribute-always-inline)
          (progn
            (llvm-sys:remove-fn-attr f 'llvm-sys:attribute-always-inline)
            (setf inline-functions (cons f inline-functions)))))
    inline-functions))


(defun call-sites-to-always-inline (module)
  (let (call-sites
        (functions (llvm-sys:module-get-function-list module)))
    (dolist (func functions)
      (let ((basic-blocks (llvm-sys:basic-blocks func)))
        (dolist (bb basic-blocks)
          (let ((instructions (llvm-sys:instructions bb)))
            (dolist (instr instructions)
              (if (or (llvm-sys:call-inst-p instr)
                      (llvm-sys:invoke-inst-p instr))
                  (let ((call-func (llvm-sys:get-called-function instr)))
                    (setq call-sites (cons instr call-sites))
                    (if (llvm-sys:has-fn-attribute call-func 'llvm-sys:attribute-always-inline)
                        (setq call-sites (cons instr call-sites))))))))))
    call-sites))
         

;;; ------------------------------------------------------------
;;;
;;; Using the new ORC JIT engine
;;;
(progn
  (defvar *jit-engine* (make-cxx-object 'llvm-sys:clasp-jit))
  #+threads(defvar *jit-engine-mutex* (mp:make-lock :name 'jit-engine-mutex :recursive t))
  (export '(jit-add-module-return-function jit-add-module-return-dispatch-function jit-remove-module))
  (defvar *builtins-module* nil)
  (defvar *fastgf-module* nil)
  (defparameter *fastgf-dump-module* nil)
  (defvar *declare-dump-module* t)
  (defvar *jit-repl-module-handles* nil)
  (defvar *jit-fastgf-module-handles* nil)
  (defvar *jit-dump-module-before-optimizations* nil)
  (defvar *jit-symbol-info* nil)
  (defvar *jit-saved-symbol-info* (make-hash-table :test #'equal :thread-safe t))
  )
(progn
  (defun jit-add-module-return-function (original-module repl-fn startup-fn shutdown-fn literals-list)
    ;; Link the builtins into the module and optimize them
    (jit-link-builtins-module original-module)
    (if *jit-dump-module-before-optimizations*
        (llvm-sys:dump-module original-module))
    #+(or)(optimize-module-for-compile original-module)
    #+(or)(quick-module-dump original-module "module after-optimize")
    (let ((module original-module))
      ;;#+threads(mp:get-lock *jit-engine-mutex*)
      ;;    (bformat t "In jit-add-module-return-function dumping module\n")
      ;;    (llvm-sys:print-module-to-stream module *standard-output*)
      (if *declare-dump-module*
          (progn
            (core:bformat t "Dumping module\n")
            (llvm-sys:dump-module module)))
      (let* ((repl-name (llvm-sys:get-name repl-fn))
             (startup-name (llvm-sys:get-name startup-fn))
             (shutdown-name (llvm-sys:get-name shutdown-fn))
             (handle (llvm-sys:clasp-jit-add-module *jit-engine* module)))
        (setq *jit-repl-module-handles* (cons handle *jit-repl-module-handles*))
;;;      (bformat t "    repl-name -> |%s|\n" repl-name)
        (let ((*jit-symbol-info* nil))
          (let ((fn (llvm-sys:jit-finalize-repl-function *jit-engine* handle repl-name startup-name shutdown-name literals-list repl-fn nil)))
            #++(bformat t "Dump of symbols associated with compilation\n")
            (dolist (e *jit-symbol-info*)
              (core:hash-table-setf-gethash *jit-saved-symbol-info* (car e) (cdr e))
              #++(bformat t "%s\n" e))
            fn)))))

  (eval-when (:execute :load-toplevel)
    (setq *fastgf-dump-module* (member :fastgf-dump-module *features*)))
  
  (defun jit-add-module-return-dispatch-function (original-module dispatch-fn startup-fn shutdown-fn literals-list)
    (jit-link-fastgf-module original-module)
    (let ((module original-module))
      (if *fastgf-dump-module* (llvm-sys:dump-module module))
      (let* ((dispatch-name (llvm-sys:get-name dispatch-fn))
             (startup-name (llvm-sys:get-name startup-fn))
             (shutdown-name (llvm-sys:get-name shutdown-fn))
             (handle (llvm-sys:clasp-jit-add-module *jit-engine* module)))
        (setq *jit-fastgf-module-handles* (cons handle *jit-fastgf-module-handles*))
        (let ((*jit-symbol-info* nil))
          (let ((fn (llvm-sys:jit-finalize-dispatch-function *jit-engine* handle dispatch-name startup-name shutdown-name literals-list)))
            (dolist (e *jit-symbol-info*)
              (core:hash-table-setf-gethash *jit-saved-symbol-info* (car e) (cdr e)))
            fn)))))
      
  (defun jit-remove-module (handle)
    (llvm-sys:clasp-jit-remove-module *jit-engine* handle))

;;; Maybe add more jit engine code here
  )

