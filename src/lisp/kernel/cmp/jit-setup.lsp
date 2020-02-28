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

;; bound when a new thread is created
(defvar *primitives*)
(export '*primitives*)

;;; Bound thread-local when the builtins module is needed
(defvar *thread-local-builtins-module* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Thread-local special variables to support the LLVM compiler
;;;
;;; To allow LLVM to run in different threads, certain things need to be thread local
;;;
(eval-when (:load-toplevel :execute)
  (mp:push-default-special-binding 'cmp:*thread-safe-context* '(llvm-sys:create-thread-safe-context))
  (mp:push-default-special-binding 'cmp:*primitives* nil)
  (mp:push-default-special-binding '*debugger-hook* nil)
  (mp:push-default-special-binding 'core::*handler-clusters* nil)
  (mp:push-default-special-binding 'core::*restart-clusters* nil)
  (mp:push-default-special-binding 'core::*condition-restarts* nil)
  (mp:push-default-special-binding 'cmp::*thread-local-builtins-module* nil)
  ;;; more thread-local special variables may be added in the future
  )

(defun thread-local-llvm-context ()
  ;; (core:bformat t "*thread-safe-context* -> %s%N" *thread-safe-context*)
  (llvm-sys:get-context *thread-safe-context*))
(export 'thread-local-llvm-context)


(defun dump-function (func)
  (warn "Do something with dump-function"))
(export 'dump-function)


(defun load-bitcode (filename &key print)
  (if *use-human-readable-bitcode*
      (let* ((input-name (make-pathname :type "ll" :defaults (pathname filename))))
        (if print (core:bformat t "Loading %s%N" input-name))
        (llvm-sys:load-bitcode-ll input-name (thread-local-llvm-context)))
      (let ((input-name (make-pathname :type "bc" :defaults (pathname filename))))
        (if print (core:bformat t "Loading %s%N" input-name))
        (llvm-sys:load-bitcode input-name (thread-local-llvm-context)))))

(defun parse-bitcode (filename context &key print)
  ;; Load a module from a bitcode or .ll file
  (if *use-human-readable-bitcode*
      (let ((input-name (make-pathname :type "ll" :defaults (pathname filename))))
        (if print (core:bformat t "Loading %s%N" input-name))
        (llvm-sys:parse-irfile input-name context))
      (let ((input-name (make-pathname :type "bc" :defaults (pathname filename))))
        (if print (core:bformat t "Loading %s%N" input-name))
        (llvm-sys:parse-bitcode-file input-name context))))

(export '(write-bitcode load-bitcode parse-bitcode))

(defun get-builtins-module ()
  (if *thread-local-builtins-module*
      *thread-local-builtins-module*
    (let* ((builtins-bitcode-name (namestring (truename (build-inline-bitcode-pathname :compile :builtins))))
           (thread-local-builtins-module (llvm-sys:parse-bitcode-file builtins-bitcode-name (thread-local-llvm-context))))
      (llvm-sys:remove-useless-global-ctors thread-local-builtins-module)
      (setq *thread-local-builtins-module* thread-local-builtins-module)
      thread-local-builtins-module)))


(defun get-builtin-target-triple-and-data-layout ()
  "Uses *features* to generate the target triple for the current machine
using features defined in corePackage.cc"
  (let ((builtins-module (get-builtins-module)))
    (values (llvm-sys:get-target-triple builtins-module) (llvm-sys:get-data-layout-str builtins-module))))

(defun llvm-create-module (name)
  (let ((m (llvm-sys:make-module (string name) (thread-local-llvm-context))))
    (multiple-value-call (function (lambda (target-triple data-layout)
                           (llvm-sys:set-target-triple m target-triple)
                           (llvm-sys:set-data-layout.string m data-layout)
                           (llvm-sys:emit-version-ident-metadata m)
                           m))
      (get-builtin-target-triple-and-data-layout))))

(defvar *run-time-module-counter* 1)
(defun next-run-time-module-name ()
  "Return the next module name"
  (prog1
      (bformat nil "module%s" *run-time-module-counter*)
    (setq *run-time-module-counter* (+ 1 *run-time-module-counter*))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((module (llvm-create-module (next-run-time-module-name)))
         (data-layout (llvm-sys:get-data-layout module)))
    (defvar *system-data-layout* data-layout)))

(defun create-run-time-module-for-compile ()
  "Run time modules are used by COMPILE - a new one needs to be created for every COMPILE.
Return the module and the global variable that represents the load-time-value-holder as
\(value module global-run-time-values function-pass-manager\)."
  (llvm-create-module (next-run-time-module-name)))

(llvm-sys:initialize-native-target)

(defvar *run-time-module* nil)

(defvar *the-module*) ; nil "This stores the module into which compile puts its stuff")
(defvar *the-function-pass-manager* nil "the function-pass-manager applied to runtime functions")


(defvar *the-module-dibuilder* nil
  "Keeps track of the current DIBuilder for generating DWARF debugging information for *the-module*.
No DIBuilder is defined for the default module")

(defun jit-constant-pointer-null-get (type)
  (llvm-sys:constant-pointer-null-get type))

(defun jit-constant-true ()
  (llvm-sys:get-true (thread-local-llvm-context)))

(defun jit-constant-false ()
  (llvm-sys:get-false (thread-local-llvm-context)))

(defun jit-constant-i<bit-width> (val &key bit-width)
  "Create an i<numbits> constant in the current context"
  (unless bit-width (error "You must provide a bit-width"))
  (let ((ap-arg (llvm-sys:make-apint-width val bit-width nil)))
    (llvm-sys:constant-int-get (thread-local-llvm-context) ap-arg)))

(defun jit-constant-i1 (val)
  "Create an i1 constant in the current context"
  (let ((ap-arg (llvm-sys:make-apint1 val)))
    (llvm-sys:constant-int-get (thread-local-llvm-context) ap-arg)))


(defun jit-constant-i3 (val)
  "Create an i3 constant in the current context"
    (let ((ap-arg (llvm-sys:make-apint-width val 3 nil)))
      (llvm-sys:constant-int-get (thread-local-llvm-context) ap-arg)))
 
(defun jit-constant-i8 (val)
  "Create an i1 constant in the current context"
  (let ((ap-arg (llvm-sys:make-apint-width val 8 nil)))
    (llvm-sys:constant-int-get (thread-local-llvm-context) ap-arg)))

(defun jit-constant-i32 (val)
  "Create a signed i32 constant in the current context"
  (let ((ap-arg (llvm-sys:make-apint-width val 32 t)))
    (llvm-sys:constant-int-get (thread-local-llvm-context) ap-arg)))


(defun jit-constant-i32-vector-ptr (vals)
  (let* ((constant-data-array (llvm-sys:constant-data-array-get-uint32 (thread-local-llvm-context) vals))
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
    (llvm-sys:constant-int-get (thread-local-llvm-context) ap-arg)))

(defun ensure-jit-constant-i64 (val)
  "Create an i64 constant in the current context if the val is a fixnum - otherwise it should already be a value"
  (if (fixnump val)
      (let ((ap-arg (llvm-sys:make-apint-width val 64 t)))
        (llvm-sys:constant-int-get (thread-local-llvm-context) ap-arg))
      val))

(defun jit-constant-ui32 (val)
  "Create an unsigned i32 constant in the current context"
  (let ((ap-arg (llvm-sys:make-apint-width val 32 nil)))
    (llvm-sys:constant-int-get (thread-local-llvm-context) ap-arg)))

(defun jit-constant-ui64 (val)
  "Create an unsigned i64 constant in the current context"
  (let ((ap-arg (llvm-sys:make-apint-width val 64 nil)))
    (llvm-sys:constant-int-get (thread-local-llvm-context) ap-arg)))


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


(defun module-make-global-string (str &optional (label ""))
  "A function for creating unique strings within the module - return an LLVM pointer to the string"
  (or *the-module* (error "module-make-global-string-ptr *the-module* is NIL"))
  (let* ((unique-string-global-variable
          (llvm-sys:get-or-create-uniqued-string-global-variable
           *the-module* str (bformat nil ":::global-str-%s" str)))
         (string-type (llvm-sys:array-type-get (llvm-sys:type-get-int8-ty (thread-local-llvm-context)) (1+ (length str)))))
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
;;; FIXME: Get the filename from source-pos-info also.
(defun function-name-from-source-info (lname)
  (if *current-source-pos-info*
      (let ((lineno (core:source-pos-info-lineno *current-source-pos-info*)))
        (cond
          (*compile-file-pathname*
           (core:bformat nil "___LAMBDA___%s.%s-%s^%d^%d"
                         (pathname-name *compile-file-pathname*)
                         (pathname-type *compile-file-pathname*)
                         *compile-file-unique-symbol-prefix*
                         lineno
                         (sys:next-number)))
          ;; Is this even possible?
          (*load-pathname*
           (core:bformat nil "%s.%s^%d^TOP-LOAD-%d"
                         (pathname-name *load-pathname*)
                         (pathname-type *load-pathname*)
                         lineno
                         (sys:next-number)))
          (t
           (core:bformat nil "UNKNOWN^%d^TOP-UNKNOWN" lineno))))
      "UNKNOWN??LINE^TOP-UNKNOWN"))

(defun jit-repl-function-name ()
  (sys:bformat nil "JITREPL-%d" (sys:next-number)))
  
(defun jit-startup-function-name ()
  (let ((next-id (sys:next-number)))
    (values (sys:bformat nil "%s-%d" sys:*module-startup-function-name* next-id) next-id)))

(defun jit-shutdown-function-name ()
  (sys:bformat nil "%s-%d" sys:*module-shutdown-function-name* (sys:next-number)))

(export '(jit-startup-function-name jit-shutdown-function-name jit-repl-function-name))

(defun jit-function-name (lname &key (compile-file-unique-symbol-prefix *compile-file-unique-symbol-prefix*))
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
       (t lname)))
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
            (pkg-symbol (symbol-package setf-name-symbol))
            ;;; e.g. lname = (METHOD (SETF #:G4336) (T CONS))
            (pkg-name (if pkg-symbol
                          (string (package-name pkg-symbol))
                          "UNINTERNED"))
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

(defun code-model (&key jit (target-faso-file (or cmp:*generate-faso* cmp:*compile-file-parallel*)))
  "Return the code-model for the compilation mode"
  (multiple-value-bind (llvm-version-string llvm-version-val)
      (ext:llvm-version)
    (if (and target-faso-file (null jit))
        (progn
          (if (>= llvm-version-val 10)
              (warn "By llvm-version 10 we should not need to use the code-model-large"))
          #+(or)(format t "llvm-version-val ~a Using llvm-sys:code-model-large cmp:*generate-faso* -> ~a   cmp:*compile-file-parallel* -> ~a~%" llvm-version-val *generate-faso* *compile-file-parallel*)
          'llvm-sys:code-model-large
          )
        (progn
          #+(or)(format t "Using llvm-sys:code-model-small cmp:*generate-faso* -> ~a   cmp:*compile-file-parallel* -> ~a~%" *generate-faso* *compile-file-parallel*)
          'llvm-sys:code-model-small))))

(export 'code-model)

(defvar *size-level* 1)

(defun optimize-module-for-compile-file (module &optional (optimize-level *optimization-level*) (size-level *size-level*))
  (declare (type (or null llvm-sys:module) module))
  (when (> *optimization-level* 0)
    (quick-module-dump module "in-optimize-module-for-compile-file-after-link-builtins")
    (let* ((pass-manager-builder (llvm-sys:make-pass-manager-builder))
           (mpm (llvm-sys:make-pass-manager))
           (fpm (llvm-sys:make-function-pass-manager module))
           (olevel optimize-level))
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
  module)

#+(or)
(defun optimize-module-for-compile (module &optional (optimize-level *optimization-level*) (size-level *size-level*))
  (declare (type (or null llvm-sys:module) module))
  (when (> *optimization-level* 0)
    #++(let ((call-sites (call-sites-to-always-inline module)))
      (bformat t "Call-sites -> %s%N" call-sites))
    (let* ((pass-manager-builder (llvm-sys:make-pass-manager-builder))
           (mpm (llvm-sys:make-pass-manager))
           (fpm (llvm-sys:make-function-pass-manager module))
           (olevel optimize-level))
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
            (setq inline-functions (cons f inline-functions)))))
    inline-functions))

(defun do-track-llvm-time (closure)
  "Run the closure in and keep track of the time, adding it to this threads accumulated llvm time"
  (let ((start-llvm-time (get-internal-run-time)))
    (unwind-protect
         (funcall closure)
      (let ((llvm-time (/ (- (get-internal-run-time) start-llvm-time) (float internal-time-units-per-second))))
        (llvm-sys:accumulate-llvm-usage-seconds llvm-time)))))

(si::fset 'with-track-llvm-time
	   #'(lambda (args env)
               (declare (core:lambda-name with-track-llvm-time))
               (let ((code (cdr args)))
                 `(do-track-llvm-time
                      (function
                       (lambda ()
                        ,@code)))))
	  t)


#+(or)(defun link-inline-remove-builtins (module)
  (when (>= *optimization-level* 2)
    (with-track-llvm-time
        (link-builtins-module module)
      (optimize-module-for-compile-file module)
      #+(or)(remove-llvm.used-if-exists module)
      (llvm-sys:remove-always-inline-functions module))))

(defun switch-always-inline-to-inline (module)
  (let ((functions (llvm-sys:module-get-function-list module))
        inline-functions)
    (dolist (f functions)
      (if (llvm-sys:has-fn-attribute f 'llvm-sys:attribute-always-inline)
          (progn
            (llvm-sys:remove-fn-attr f 'llvm-sys:attribute-always-inline)
            (llvm-sys:add-fn-attr f 'llvm-sys:attribute-inline-hint)
            (setq inline-functions (cons f inline-functions)))))
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
#+threads(defvar *jit-log-lock* (mp:make-recursive-mutex 'jit-log-lock))
(defvar *jit-log-stream*)
(defvar *jit-pid*)

(defun jit-register-symbol (symbol-name-string symbol-info)
  "This is a callback from llvmoExpose.cc::save_symbol_info for registering JITted symbols"
  (core:hash-table-setf-gethash *jit-saved-symbol-info* symbol-name-string symbol-info)
  (if (member :jit-log-symbols *features*)
      (unwind-protect
           (progn
             #+threads(mp:get-lock *jit-log-lock*)
             (cond
               ;; If this is the first process to generate a symbol then create the master symbol file
               ((not (boundp '*jit-pid*))
                (setq *jit-pid* (core:getpid))
                (let ((filename (core:bformat nil "/tmp/perf-%s.map" (core:getpid))))
                  (core:bformat *error-output* "Writing jitted symbols to %s%N" filename)
                  (setq *jit-log-stream* (open filename :direction :output))))
               ;; If we are in a forked child then we need to create a new clasp-symbols-<pid> file and
               ;; refer to the parent clasp-symbols-<ppid> file.
               ((and *jit-log-stream* (not (= *jit-pid* (core:getpid))))
                (close *jit-log-stream*) ; Shut down symbols for forked children
                (setq *jit-log-stream* nil)))
             (if *jit-log-stream*
                 (progn
                   (write (core:pointer-integer (cadr symbol-info)) :base 16 :stream *jit-log-stream*)
                   (write-char #\space *jit-log-stream*)
                   ;; car of symbol-info is a fixnum
                   (write (car symbol-info) :base 16 :stream *jit-log-stream* :pretty nil)
                   (write-char #\space *jit-log-stream*)
                   (write-string symbol-name-string *jit-log-stream*)
                   (terpri *jit-log-stream*)
                   (finish-output *jit-log-stream*))))
        (progn
          #+threads(mp:giveup-lock *jit-log-lock*)))))

(progn
  (export '(jit-add-module-return-function jit-add-module-return-dispatch-function jit-remove-module))
  (defparameter *jit-lock* (mp:make-recursive-mutex 'jit-lock))
  (defun jit-add-module-return-function (original-module main-fn startup-fn shutdown-fn literals-list
                                         &key output-path)
    ;; Link the builtins into the module and optimize them
    #+(or)
    (progn
      (quick-module-dump original-module "before-link-builtins")
      (link-inline-remove-builtins original-module))
    (quick-module-dump original-module "module-before-optimize")
    (let ((module original-module))
      (irc-verify-module-safe module)
      (let ((jit-engine llvm-sys:*jit-engine*)
            (repl-name (if main-fn (llvm-sys:get-name main-fn) nil))
            (startup-name (if startup-fn (llvm-sys:get-name startup-fn) nil))
            (shutdown-name (if shutdown-fn (llvm-sys:get-name shutdown-fn) nil)))
        (if (or (null repl-name) (string= repl-name ""))
            (error "Could not obtain the name of the repl function ~s - got ~s" main-fn repl-name))
        (if (or (null startup-name) (string= startup-name ""))
            (error "Could not obtain the name of the startup function ~s - got ~s" startup-fn startup-name))
        (with-track-llvm-time
            (unwind-protect
                 (progn
                   #+(or)
                   (progn
                     #+(or)(core:bformat t "*jit-lock* -> %s    jit-engine -> %s   module -> %s   *thread-safe-context* -> %s%N"
                                         *jit-lock*
                                         jit-engine
                                         module
                                         *thread-safe-context*)
                     (core:bformat t "About to dump module%N")
                     (llvm-sys:dump-module module)
                     (core:bformat t "startup-name |%s|%N" startup-name)
                     (core:bformat t "Done dump module%N")
                     )
                   (mp:get-lock *jit-lock*)
                   (llvm-sys:add-irmodule jit-engine module *thread-safe-context*)
                   (llvm-sys:jit-finalize-repl-function jit-engine repl-name startup-name shutdown-name literals-list))
              (progn
                (gctools:thread-local-cleanup)
                (mp:giveup-lock *jit-lock*))))))))
