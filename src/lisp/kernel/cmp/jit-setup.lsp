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

;;; Bound thread-local when the builtins module is needed
(defvar *thread-local-builtins-module* nil)

(defvar *thread-local-fastgf-module* nil)
(defvar *thread-local-jit-engine* nil)

;;;(defvar *llvm-context* (llvm-sys:create-llvm-context))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Thread-local special variables to support the LLVM compiler
;;;
;;; To allow LLVM to run in different threads, certain things need to be thread local
;;;
(eval-when (:load-toplevel :execute)
  (mp:push-default-special-binding 'cmp:*llvm-context* '(llvm-sys:create-llvm-context))
  (mp:push-default-special-binding 'cmp:*primitives* nil)
  (mp:push-default-special-binding 'cmp::*thread-local-builtins-module* nil)
  (mp:push-default-special-binding 'cmp::*thread-local-fastgf-module* nil)
;;;  (mp:push-default-special-binding 'cmp::*thread-local-jit-engine* nil)
  ;;; more thread-local special variables may be added in the future
  )

(defun dump-function (func)
  (warn "Do something with dump-function"))
(export 'dump-function)

(defun write-bitcode (module output-path)
  ;; Write bitcode as either .bc files or .ll files
  (if *use-human-readable-bitcode*
      (let* ((filename (make-pathname :type "ll" :defaults (pathname output-path)))
             (output-name (or (namestring filename)
                              (error "In write-bitcode the output file for ~a could not be coerced to a namestring")))
             (fout (open output-name :direction :output)))
        (unwind-protect
             (llvm-sys:dump-module module fout)
          (close fout)))
      (llvm-sys:write-bitcode-to-file module output-path)))

(defun load-bitcode (filename)
  (if *use-human-readable-bitcode*
      (let* ((input-name (make-pathname :type "ll" :defaults (pathname filename))))
        (llvm-sys:load-bitcode-ll input-name))
      (llvm-sys:load-bitcode filename)))

(defun parse-bitcode (filename context)
  ;; Load a module from a bitcode or .ll file
  (if *use-human-readable-bitcode*
      (let ((input-name (make-pathname :type "ll" :defaults (pathname filename))))
        (llvm-sys:parse-irfile input-name context))
      (llvm-sys:parse-bitcode-file filename context)))

(export '(write-bitcode load-bitcode parse-bitcode))

(defun get-builtins-module ()
  (if *thread-local-builtins-module*
      *thread-local-builtins-module*
    (let* ((builtins-bitcode-name (namestring (truename (build-inline-bitcode-pathname :compile :builtins))))
           (thread-local-builtins-module (llvm-sys:parse-bitcode-file builtins-bitcode-name *llvm-context*)))
      (llvm-sys:remove-useless-global-ctors thread-local-builtins-module)
      (setq *thread-local-builtins-module* thread-local-builtins-module)
      thread-local-builtins-module)))

(defun get-builtin-target-triple-and-data-layout ()
  "Uses *features* to generate the target triple for the current machine
using features defined in corePackage.cc"
  (let ((builtins-module (get-builtins-module)))
    (values (llvm-sys:get-target-triple builtins-module) (llvm-sys:get-data-layout-str builtins-module))))

(defun llvm-create-module (name)
  (let ((m (llvm-sys:make-module (string name) *llvm-context*)))
    (multiple-value-bind (target-triple data-layout)
        (get-builtin-target-triple-and-data-layout)
      (llvm-sys:set-target-triple m target-triple)
      (llvm-sys:set-data-layout.string m data-layout)
      (llvm-sys:emit-version-ident-metadata m)
      m)))

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

(defun ensure-jit-constant-i64 (val)
  "Create an i64 constant in the current context if the val is a fixnum - otherwise it should already be a value"
  (if (fixnump val)
      (let ((ap-arg (llvm-sys:make-apint-width val 64 t)))
        (llvm-sys:constant-int-get *llvm-context* ap-arg))
      val))

(defun jit-constant-ui32 (val)
  "Create an unsigned i32 constant in the current context"
  (let ((ap-arg (llvm-sys:make-apint-width val 32 nil)))
    (llvm-sys:constant-int-get *llvm-context* ap-arg)))

(defun jit-constant-ui64 (val)
  "Create an unsigned i64 constant in the current context"
  (let ((ap-arg (llvm-sys:make-apint-width val 64 nil)))
    (llvm-sys:constant-int-get *llvm-context* ap-arg)))


(defun jit-constant-size_t (val)
  (let ((sizeof-size_t +size_t-bits+))
    (cond
      ((= 64 sizeof-size_t) (jit-constant-i64 val))
      ((= 32 sizeof-size_t) (jit-constant-i32 val))
      (t (error "Add support for size_t sizeof = ~a" sizeof-size_t)))))

(defun jit-constant-uintptr_t (val)
  (let ((sizeof-size_t +size_t-bits+))
    (cond
      ((= 64 sizeof-size_t) (jit-constant-ui64 val))
      ((= 32 sizeof-size_t) (jit-constant-ui32 val))
      (t (error "Add support for size_t sizeof = ~a" sizeof-size_t)))))

(defun jit-constant-intptr_t (val)
  (let ((sizeof-size_t +size_t-bits+))
    (cond
      ((= 64 sizeof-size_t) (jit-constant-i64 val))
      ((= 32 sizeof-size_t) (jit-constant-i32 val))
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

(defun escape-and-join-jit-name (names)
  (let ((all (make-string-output-stream))
        name)
    (tagbody
     outer
       (let ((name (string (car names))))
         (let ((i 0)
               (len (length name)))
           (tagbody
            top
              (let ((c (aref name i)))
                (if (char= c #\\)
                    (write-string "\\\\" all)
                    (if (char= c #\^)
                        (write-string "\\^" all)
                        (write-char c all))))
              (setq i (+ 1 i))
              (if (< i len) (go top)))
           (write-char #\^ all)))
       (setq names (cdr names))
       (if names (go outer)))
    (write-char #\^ all)
    (get-output-stream-string all)))

(defun unescape-and-split-jit-name (name)
  (let* (parts
         (unescaped (make-string-output-stream))
         (i 0)
         (len (length name))
         (len-1 (- len 1)))
    (tagbody
     top
       (let ((c (aref name i))
             (cnext (if (< i len-1) (aref name (+ i 1)) #.(code-char 0))))
         (if (char= c #\\)
             (if (char= cnext #\\)
                 (progn
                   (write-string "\\" unescaped)
                   (setq i (+ 1 i)))
                 (if (char= cnext #\^)
                     (progn
                       (write-char #\^ unescaped)
                       (setq i (+ 1 i)))
                     (error "Bad escape sequence in ~a starting at ~a" name i)))
             (if (char= c #\^)
                 (setq parts (cons (get-output-stream-string unescaped) parts))
                 (write-char c unescaped))))
       (setq i (+ 1 i))
       (if (< i len) (go top)))
    (setq parts (cons (get-output-stream-string unescaped) parts))
    (nreverse parts)))


(defun print-name-from-unescaped-split-name (raw-name parts symbol)
  (let* ((end-part-pos (position "" parts :test #'equal))
         (type-name (if (numberp end-part-pos)
                        (elt parts (1- end-part-pos))
                        (car (last parts)))))
    (cond
      ((string= type-name "METHOD")
       (format nil "{~a ~a}" symbol (third parts)))
      ((string= type-name "FN")
       (format nil "~a" symbol))
      ((string= type-name "SETF")
       (format nil "(SETF ~a)" symbol))
      (t raw-name))))

(export 'print-name-from-unescaped-split-name)
(defun function-name-from-source-info (lname)
  (cond
    ((and *compile-file-pathname*
          *current-form-lineno*)
     (core:bformat nil "%s.%s^%d^TOP-COMPILE-FILE" (pathname-name *compile-file-pathname*) (pathname-type *compile-file-pathname*) *current-form-lineno*))
    ((and *load-pathname*
          *current-form-lineno*)
     (core:bformat nil "%s.%s^%d^TOP-LOAD" (pathname-name *load-pathname*) (pathname-type *load-pathname*) *current-form-lineno*))
    (*current-form-lineno*
     (core:bformat nil "UNKNOWN^%d^TOP-UNKNOWN" *current-form-lineno*))
    (t "UNKNOWN??LINE^TOP-UNKNOWN")))
  
(defun jit-function-name (lname)
  "Depending on the type of LNAME an actual LLVM name is generated"
  ;;  (break "Check backtrace")
  (cond
    ((pathnamep lname) (bformat nil "MAIN-%s" (string-upcase (pathname-name lname))))
    ((stringp lname)
     (cond
       ((string= lname core:+run-all-function-name+) lname) ; this one is ok
       ((string= lname core:+clasp-ctor-function-name+) lname) ; this one is ok
       ((string= lname "IMPLICIT-REPL") lname)  ; this one is ok
       ((string= lname "TOP-LEVEL") (function-name-from-source-info lname))
       ((string= lname "UNNAMED-LAMBDA") lname) ; this one is ok
       ((string= lname "lambda") lname)         ; this one is ok
       ((string= lname "ltv-literal") lname)    ; this one is ok
       ((string= lname "disassemble") lname)    ; this one is ok
       (t (bformat t "jit-function-name lname = %s%N" lname)
          (break "Always pass symbol as name") lname)))
    ((symbolp lname)
     (cond ((eq lname 'core::top-level)
            (function-name-from-source-info lname))
           ((eq lname 'cmp::repl)
            (function-name-from-source-info lname))
           (t 
            (let* ((sym-pkg (symbol-package lname))
                   (sym-name (symbol-name lname))
                   (pkg-name (if sym-pkg
                                 (string (package-name sym-pkg))
                                 ;;; KNPK I don't undestand why "KEYWORD" is used here
                                 ;;; (package-name (symbol-package :test)) -> "KEYWORD", so how can sym-pkg be empty for this case
                                 ;;; More likely it is an uninterned symbol
                                 "KEYWORD")))
              (escape-and-join-jit-name (list sym-name pkg-name "FN"))))))
    ((and (consp lname) (eq (car lname) 'setf) (symbolp (second lname)))
;;;     (core:bformat t "jit-function-name handling SETF: %s%N" lname)
     (let* ((sn (cadr lname))
            (sym-pkg (symbol-package sn))
            (sym-name (symbol-name sn))
            (pkg-name (if sym-pkg
                          (string (package-name sym-pkg))
                          "KEYWORD")))
       (escape-and-join-jit-name (list sym-name pkg-name "SETF"))))
    ((and (consp lname) (eq (car lname) 'setf) (consp (second lname)))
;;;     (core:bformat t "jit-function-name handling SETFCONS: %s%N" lname)
     ;; (setf (something ...))
     (let* ((sn (second lname))
            (sn-sym (first sn))
            (sym-pkg (symbol-package sn-sym))
            (sym-name (symbol-name sn-sym))
            (pkg-name (if sym-pkg
                          (string (package-name sym-pkg))
                          "KEYWORD")))
       (escape-and-join-jit-name (list sym-name pkg-name "SETFCONS"))))
    ((and (consp lname) (eq (car lname) 'method) (symbolp (second lname)))
     (let* ((symbol (second lname))
            (sym-pkg (symbol-package symbol))
            (pkg-name (if sym-pkg
                      (string (package-name sym-pkg))
                      "UNINTERNED"))
           (name (symbol-name symbol))
           (specializers (core:bformat nil "%s" (cddr lname))))
       (escape-and-join-jit-name (list name pkg-name specializers "METHOD"))))
    ((and (consp lname) (eq (car lname) 'method) (consp (second lname)) (eq (car (second lname)) 'setf))
     (let* ((name-list (second lname))
            (setf-name-symbol (second name-list))
            (pkg-name (string (package-name (symbol-package setf-name-symbol))))
            (specializers (core:bformat nil "%s" (cddr lname))))
       (escape-and-join-jit-name (list (string setf-name-symbol) pkg-name specializers "SETFMETHOD"))))
    ((consp lname)
;;;     (core:bformat t "jit-function-name handling UNKNOWN: %s%N" lname)
     ;; What is this????
     (bformat nil "%s_CONS-LNAME?" lname))
    (t (error "Illegal lisp function name[~a]" lname))))
(export '(jit-function-name unescape-and-split-jit-name escape-and-join-jit-name))


(setq core:*llvm-function-name-hook* #'jit-function-name)


;;; ------------------------------------------------------------
;;;
;;;  JIT facility
;;;


(defun link-builtins-module (module)
  "Merge the builtins module with the passed module.
The passed module is modified as a side-effect."
  ;; Clone the intrinsics module and link it in
  (quick-module-dump module "module before linking builtins-clone")
  (let ((linker (llvm-sys:make-linker module))
        (builtins-clone (llvm-sys:clone-module (get-builtins-module))))
    ;;(switch-always-inline-to-inline builtins-clone)
    (quick-module-dump builtins-clone "builtins-clone")
    (llvm-sys:link-in-module linker builtins-clone))
  module)

(defun link-fastgf-module (module)
  "Merge the intrinsics module with the passed module.
The passed module is modified as a side-effect."
  (unless *thread-local-fastgf-module*
    (let* ((fastgf-bitcode-name (namestring (truename (build-inline-bitcode-pathname :compile :fastgf))))
           (fastgf-module (llvm-sys:parse-bitcode-file fastgf-bitcode-name *llvm-context*)))
      (llvm-sys:remove-useless-global-ctors fastgf-module)
      (setf *thread-local-fastgf-module* fastgf-module)))
  ;; Clone the intrinsics module and link it in
  (quick-module-dump module "module before linking fastgf-clone")
  (let ((linker (llvm-sys:make-linker module))
        (fastgf-clone (llvm-sys:clone-module *thread-local-fastgf-module*)))
    ;;(remove-always-inline-from-functions fastgf-clone)
    (quick-module-dump fastgf-clone "fastgf-clone")
    (llvm-sys:link-in-module linker fastgf-clone))
  module)

(defun jit-engine ()
  "Lazy initialize the *thread-local-jit-engine* and return it"
  (if *thread-local-jit-engine*
      *thread-local-jit-engine*
      (setf *thread-local-jit-engine* (make-cxx-object 'llvm-sys:clasp-jit))))

(defvar *optimizations-on* t)
(defvar *optimization-level* :-O3)
(defvar *size-level* 1)

(defun foobly-file ()
  #+(or)(when (and *optimizations-on* module)
          #+(or) 1))

(defun optimize-module-for-compile-file (module &optional (optimize-level *optimization-level*) (size-level *size-level*))
  (declare (type (or null llvm-sys:module) module))
  (when (and *optimizations-on* module)
    (quick-module-dump module "in-optimize-module-for-compile-file-after-link-builtins")
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
      (progn
        (llvm-sys:pass-manager-builder-setf-inliner pass-manager-builder (llvm-sys:create-always-inliner-legacy-pass))
        (llvm-sys:populate-function-pass-manager pass-manager-builder fpm)
        (llvm-sys:populate-module-pass-manager pass-manager-builder mpm))
      #+(or)(llvm-sys:populate-ltopass-manager pass-manager-builder mpm)
      (llvm-sys:pass-manager-run mpm module))
    (llvm-sys:remove-always-inline-functions module))
  module)


(defun optimize-module-for-compile (module)
  #+(or)(bformat *debug-io* "In optimize-module-for-compile%N")
  #+(or)(llvm-sys:dump-module module)
  module)

#+(or)
(defun optimize-module-for-compile (module &optional (optimize-level *optimization-level*) (size-level *size-level*))
  (declare (type (or null llvm-sys:module) module))
  (when (and *optimizations-on* module)
    #++(let ((call-sites (call-sites-to-always-inline module)))
      (bformat t "Call-sites -> %s%N" call-sites))
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



(defun link-inline-remove-builtins (module)
  (link-builtins-module module)
  (optimize-module-for-compile-file module)
  (llvm-sys:remove-always-inline-functions module))

(defun link-inline-remove-fastgf (module)
  (link-fastgf-module module)
  (optimize-module-for-compile-file module)
  (llvm-sys:remove-always-inline-functions module))

(defun switch-always-inline-to-inline (module)
  (let ((functions (llvm-sys:module-get-function-list module))
        inline-functions)
    (dolist (f functions)
      (if (llvm-sys:has-fn-attribute f 'llvm-sys:attribute-always-inline)
          (progn
            (llvm-sys:remove-fn-attr f 'llvm-sys:attribute-always-inline)
            (llvm-sys:add-fn-attr f 'llvm-sys:attribute-inline-hint)
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

;;; jit-register-symbol is a call
#+threads(defvar *jit-log-lock* (mp:make-lock :name 'jit-log-lock :recursive t))
(defvar *jit-log-stream*)

(defun jit-register-symbol (symbol-name-string symbol-info)
  "This is a callback from llvmoExpose.cc::save_symbol_info for registering JITted symbols"
  (core:hash-table-setf-gethash *jit-saved-symbol-info* symbol-name-string symbol-info)
  (if (member :jit-log-symbols *features*)
      (unwind-protect
           (progn
             #+threads(mp:lock *jit-log-lock* t)
             (if (not (boundp '*jit-log-stream*))
                 (let ((filename (core:bformat nil "/tmp/clasp-symbols-%s" (core:getpid))))
                   (core:bformat *debug-io* "Writing jitted symbols to %s%N" filename)
                   (setq *jit-log-stream* (open filename :direction :output))))
             (write-string (core:pointer-as-string (cadr symbol-info)) *jit-log-stream*)
             (write-char #\space *jit-log-stream*)
             ;; car of symbol-info is a fixnum
             (write (car symbol-info) :stream *jit-log-stream* :pretty nil)
             (write-char #\space *jit-log-stream*)
             (write-string symbol-name-string *jit-log-stream*)
             (terpri *jit-log-stream*)
             (finish-output *jit-log-stream*))
        (progn
          #+threads(mp:unlock *jit-log-lock*)))))

(progn
  (export '(jit-add-module-return-function jit-add-module-return-dispatch-function jit-remove-module))
  (defparameter *jit-lock* (mp:make-lock :name 'jit-lock :recursive t))
  (defun jit-add-module-return-function (original-module main-fn startup-fn shutdown-fn literals-list &optional dispatcher )
    ;; Link the builtins into the module and optimize them
    (if (null dispatcher)
        (progn
          (quick-module-dump original-module "before-link-builtins")
          (link-inline-remove-builtins original-module)
          (quick-module-dump original-module "module-before-optimize"))
        (progn
          (link-inline-remove-fastgf original-module)))
    (let ((module original-module))
      ;; (irc-verify-module-safe module)
      (let ((jit-engine (jit-engine))
            (repl-name (llvm-sys:get-name main-fn))
            (startup-name (llvm-sys:get-name startup-fn))
            (shutdown-name (llvm-sys:get-name shutdown-fn))
            (start-llvm-time (get-internal-run-time)))
        (unwind-protect
             (progn
               (mp:lock *jit-lock* t)
               (let ((handle (llvm-sys:clasp-jit-add-module jit-engine module)))
                 (llvm-sys:jit-finalize-repl-function jit-engine handle repl-name startup-name shutdown-name literals-list)))
          (let ((llvm-time (/ (- (get-internal-run-time) start-llvm-time) (float internal-time-units-per-second))))
            (llvm-sys:accumulate-llvm-usage-seconds llvm-time)
            (mp:unlock *jit-lock*))))))

  (defun jit-add-module-return-dispatch-function (original-module dispatch-fn startup-fn shutdown-fn literals-list)
    (jit-add-module-return-function original-module dispatch-fn startup-fn shutdown-fn literals-list :dispatch))
     
  (defun jit-remove-module (handle)
    (llvm-sys:clasp-jit-remove-module (jit-engine) handle)))

