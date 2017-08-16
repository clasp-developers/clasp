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

;;;(defvar *llvm-context* (llvm-sys:create-llvm-context))
(mp:push-default-special-binding 'cmp:*llvm-context* '(llvm-sys:create-llvm-context))



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


;;; ------------------------------------------------------------
;;;
;;; Using the new ORC JIT engine
;;;
(progn
  (defvar *jit-engine* (make-cxx-object 'llvm-sys:clasp-jit))
  #+threads(defvar *jit-engine-mutex* (mp:make-lock :name 'jit-engine-mutex :recursive t))
  (export '(jit-add-module-return-function jit-add-module-return-dispatch-function jit-remove-module))
  (defvar *intrinsics-module* nil)
  (defparameter *jit-dump-module* nil)
  (defvar *declare-dump-module* nil)
  (defvar *jit-repl-module-handles* nil)
  (defvar *jit-fastgf-module-handles* nil)
  (defun jit-add-module-return-function (original-module repl-fn startup-fn shutdown-fn literals-list)
    (let ((module (llvm-sys:clone-module original-module)))
      (unwind-protect
           (progn
             #+threads(mp:get-lock *jit-engine-mutex*)
             ;;    (bformat t "In jit-add-module-return-function dumping module\n")
             ;;    (llvm-sys:print-module-to-stream module *standard-output*)
             (if *jit-dump-module* (llvm-sys:dump module))
             (if *declare-dump-module* (llvm-sys:dump module))
             (let* ((repl-name (llvm-sys:get-name repl-fn))
                    (startup-name (llvm-sys:get-name startup-fn))
                    (shutdown-name (llvm-sys:get-name shutdown-fn))
                    (handle (llvm-sys:clasp-jit-add-module *jit-engine* module)))
               (setq *jit-repl-module-handles* (cons handle *jit-repl-module-handles*))
;;;      (bformat t "    repl-name -> |%s|\n" repl-name)
               (llvm-sys:jit-finalize-repl-function *jit-engine* handle repl-name startup-name shutdown-name literals-list repl-fn nil)))
        (progn
          #+threads(mp:giveup-lock *jit-engine-mutex*)))))

  (defun jit-add-module-return-dispatch-function (original-module dispatch-fn startup-fn shutdown-fn literals-list)
    (let ((module (llvm-sys:clone-module original-module)))
      (unwind-protect
           (progn
             #+threads(mp:get-lock *jit-engine-mutex*)
             (let* ((dispatch-name (llvm-sys:get-name dispatch-fn))
                    (startup-name (llvm-sys:get-name startup-fn))
                    (shutdown-name (llvm-sys:get-name shutdown-fn))
                    (handle (llvm-sys:clasp-jit-add-module *jit-engine* module)))
               (setq *jit-fastgf-module-handles* (cons handle *jit-fastgf-module-handles*))
               (llvm-sys:jit-finalize-dispatch-function *jit-engine* handle dispatch-name startup-name shutdown-name literals-list)))
        (progn
          #+threads(mp:giveup-lock *jit-engine-mutex*)))))
      
  (defun jit-remove-module (handle)
    (unwind-protect
         (progn
           #+threads(mp:get-lock *jit-engine-mutex*)
           (llvm-sys:clasp-jit-remove-module *jit-engine* handle))
      (progn
        #+threads(mp:giveup-lock *jit-engine-mutex*))))
  )

;;; Use old execution engine approach
#+(or)
(progn
  (defun jit-add-module-return-function (module repl-fn startup-fn shutdown-fn literals-list)
    "Add the module to the jit and return a handle"
    (unwind-protect
         (progn
           #+threads(mp:get-lock *jit-engine-mutex*)
           (if (not *jit-engine*)
               (setq *jit-engine* (create-run-time-execution-engine module))
               (llvm-sys:add-module *jit-engine* module))
           (llvm-sys:finalize-engine-and-register-with-gc-and-get-compiled-function
            *jit-engine*
            "JIT"
            repl-fn
            (irc-environment-activation-frame nil) 0 0 0
            startup-fn
            shutdown-fn
            literals-list))
      (progn
        #+threads(mp:giveup-lock *jit-engine-mutex*))))

  (defun jit-add-module-return-dispatch-function (module repl-fn startup-fn shutdown-fn literals-list)
    "Add the module to the jit and return a handle"
    (irc-verify-function repl-fn)
    (unwind-protect
         (progn
           #+threads(mp:get-lock *jit-engine-mutex*)
           (if (not *jit-engine*)
               (setq *jit-engine* (create-run-time-execution-engine module))
               (llvm-sys:add-module *jit-engine* module))
           (llvm-sys:finalize-engine-and-get-dispatch-function
            *jit-engine*
            "JIT-DISPATCH"
            repl-fn
            startup-fn
            shutdown-fn
            literals-list))
      (progn
        #+threads(mp:giveup-lock *jit-engine-mutex*)
        )))

  (defun jit-remove-module (handle)
    "Maybe remove the module"
    nil))
