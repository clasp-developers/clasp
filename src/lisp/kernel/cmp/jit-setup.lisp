;;;
;;;    File: jit-setup.lisp
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

(defconstant +debug-dwarf-version+ 5)

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
  (mp:push-default-special-binding '*debugger-hook* nil)
  (mp:push-default-special-binding 'core::*handler-clusters* nil)
  (mp:push-default-special-binding 'core::*restart-clusters* nil)
  (mp:push-default-special-binding 'core::*condition-restarts* nil)
  (mp:push-default-special-binding 'cmp::*thread-local-builtins-module* nil)
  ;;; more thread-local special variables may be added in the future
  )

(defun thread-local-llvm-context ()
  ;; (core:fmt t "*thread-safe-context* -> {}%N" *thread-safe-context*)
  (llvm-sys:thread-local-llvm-context))

(export 'thread-local-llvm-context)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Image save load support
;;;
;;; Provide a function for image-save to prepare for the save
;;;
;;; And another to bring the system back up.
;;;


(defun register-save-hook (hook)
  (setq sys:*save-hook* (cons hook sys:*save-hook*)))

(defun invoke-save-hooks ()
  (format t "Running ~d sys:*save-hooks*~%" (length sys:*save-hook*))
  (let ((cur sys:*save-hook*))
    (tagbody
     top (if (consp cur)
             (progn
               (funcall (car cur))
               (setq cur (cdr cur))
               (go top)))))
  (format t "Finished cmp:invoke-save-hooks~%"))

(defun snapshot-load-restore ()
  )

(export '(snapshot-load-restore register-save-hook))

(defun dump-function (func)
  (declare (ignore func))
  (warn "Do something with dump-function"))
(export 'dump-function)

(defun dso-handle-module (dylib)
  (let* ((dso-handle-name "__dso_handle")
         (module (llvm-create-module dso-handle-name)))
    (llvm-sys:make-global-variable
     module
     %i32%                              ; type
     nil                                ; isConstant
     'llvm-sys:external-linkage         ; linkage
     (jit-constant-i32 0)
     dso-handle-name)
    (llvm-sys:add-irmodule (llvm-sys:clasp-jit)
                           dylib
                           module
                           *thread-safe-context*)
    module))


(defun load-ir-run-c-function (filename function-name)
  (let* ((pathname (merge-pathnames (pathname filename)))
         (bc-file (make-pathname :type "bc" :defaults pathname))
         (ll-file (make-pathname :type "ll" :defaults pathname))
         (module (if (probe-file ll-file)
                     (llvm-sys:parse-irfile ll-file (thread-local-llvm-context))
                     (if (probe-file bc-file)
                         (llvm-sys:parse-bitcode-file bc-file (thread-local-llvm-context))
                         (error "Could not find file ~a or ~a" ll-file bc-file))))
         (dylib (llvm-sys:create-and-register-jitdylib (llvm-sys:clasp-jit) (namestring pathname))))
    (dso-handle-module dylib)
    (llvm-sys:add-irmodule (llvm-sys:clasp-jit) dylib module *thread-safe-context*)
    (llvm-sys:jit-finalize-run-cxx-function (llvm-sys:clasp-jit) dylib function-name)))


(defun load-bitcode (filename &key print clasp-build-mode)
  (cond
    ((eq clasp-build-mode :bitcode)
     (if *use-human-readable-bitcode*
         (let* ((input-name (make-pathname :type "ll" :defaults (pathname filename))))
           (if print (core:fmt t "Loading {}%N" input-name))
           (llvm-sys:load-ll input-name (thread-local-llvm-context)))
         (let ((input-name (make-pathname :type "bc" :defaults (pathname filename))))
           (if print (core:fmt t "Loading {}%N" input-name))
           (llvm-sys:load-bc input-name (thread-local-llvm-context)))))
    (t (error "Add support for load-bitcode with clasp-build-mode of ~a" clasp-build-mode))))

(defun parse-bitcode (filename context &key print clasp-build-mode)
  ;; Load a module from a bitcode or .ll file
  (cond
    ((eq clasp-build-mode :fasobc)
     (if print (core:fmt t "Loading {}%N" filename))
     (llvm-sys:parse-bitcode-file filename context))
    ((eq clasp-build-mode :fasoll)
     (if print (core:fmt t "Loading {}%N" filename))
     (llvm-sys:parse-irfile filename context))
    ((eq clasp-build-mode :bitcode)
     (if *use-human-readable-bitcode*
         (let ((input-name (make-pathname :type "ll" :defaults (pathname filename))))
           (if print (core:fmt t "Loading {}%N" input-name))
           (llvm-sys:parse-irfile input-name context))
         (let ((input-name (make-pathname :type "bc" :defaults (pathname filename))))
           (if print (core:fmt t "Loading {}%N" input-name))
           (llvm-sys:parse-bitcode-file input-name context))))
    (t (error "Add support for clasp-build-mode ~a" clasp-build-mode))))

(export '(write-bitcode load-bitcode parse-bitcode load-ir-run-c-function))

(defun get-builtin-target-triple-and-data-layout ()
  "Query llvm for the target triple and the data-layout"
  (let* ((triple-str (llvm-sys:get-default-target-triple))
         (target (llvm-sys:target-registry-lookup-target "" (llvm-sys:make-triple triple-str)))
         (target-machine (llvm-sys:create-target-machine target
                                                         triple-str
                                                         ""
                                                         ""
                                                         (llvm-sys:make-target-options)
                                                         'llvm-sys:reloc-model-pic-
                                                         (code-model)
                                                         'llvm-sys:code-gen-opt-default
                                                         nil))
         (data-layout (llvm-sys:create-data-layout target-machine))
         (data-layout-str (llvm-sys:get-string-representation data-layout)))
    (values triple-str data-layout-str data-layout)))

(defun llvm-create-module (name)
  (let ((m (llvm-sys:make-module (string name) (thread-local-llvm-context))))
    (multiple-value-call (function (lambda (target-triple data-layout-str &rest dummy)
                           (declare (ignore dummy))
                           (llvm-sys:set-target-triple m target-triple)
                           (llvm-sys:set-data-layout.string m data-layout-str)
                           (llvm-sys:emit-version-ident-metadata m)
                           m))
      (get-builtin-target-triple-and-data-layout))))


(defun link-bitcode-modules-together (output-pathname part-pathnames
                                      &key additional-bitcode-pathnames
                                        clasp-build-mode)
  "Link a bunch of modules together, return the linked module"
  (let* ((link-module (llvm-create-module (pathname-name output-pathname))))
    (let* ((part-index 1))
      ;; Don't enforce .bc extension for additional-bitcode-pathnames
      ;; This is where I used to link the additional-bitcode-pathnames
      (let ((part-pn nil))
        (tagbody
         top
           (if (null part-pathnames) (go done))
           (setq part-pn (car part-pathnames))
           (setq part-pathnames (cdr part-pathnames))
           #+(or)(format t "part-pn: ~a  exists: ~a   size: ~a~%"
                   part-pn
                   (probe-file part-pn)
                   (let ((fin (open part-pn :direction :input)))
                     (prog1
                         (file-length fin)
                       (close fin))))
           (let* ((part-module (parse-bitcode (namestring (truename part-pn)) (thread-local-llvm-context)
                                              :clasp-build-mode clasp-build-mode)))
             (setq part-index (+ 1 part-index))
             (multiple-value-call (function (lambda (failure error-msg)
                                    (if failure
                                        (progn
                                          (format t "While linking part module: ~a  encountered error: ~a~%" part-pn error-msg)))))
               (llvm-sys:link-modules link-module part-module)))
           (go top)
         done))
      ;; The following links in additional-bitcode-pathnames
      (let (part-pn)
        (tagbody
         top
           (if (null additional-bitcode-pathnames) (go done))
           (setq part-pn (car additional-bitcode-pathnames))
           (setq additional-bitcode-pathnames (cdr additional-bitcode-pathnames))
           (let* ((bc-file part-pn)
                  (part-module (llvm-sys:parse-bitcode-file (namestring (truename bc-file)) (thread-local-llvm-context))))
             (multiple-value-call (function (lambda (failure error-msg)
                                    (if failure
                                        (progn
                                          (format t "While linking part module: ~a  encountered error: ~a~%" part-pn error-msg)))))
               (llvm-sys:link-modules link-module part-module)))
           (go top)
         done))
      link-module)))
(export 'link-bitcode-modules-together)

(defvar *run-time-module-counter* 1)
(defun next-run-time-module-name ()
  "Return the next module name"
  (prog1
      (core:fmt nil "module{}" *run-time-module-counter*)
    (setq *run-time-module-counter* (+ 1 *run-time-module-counter*))))

#+(or)(defvar *the-system-data-layout*)
(defun system-data-layout ()
  (multiple-value-bind (triple data-layout-str data-layout)
      (get-builtin-target-triple-and-data-layout)
    (declare (ignore triple data-layout-str))
    data-layout))

(export 'system-data-layout)

#+(or)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-save-hook (function (lambda () (makunbound '*the-system-data-layout*)))))

(defun load-object-files (&optional (object-files core:*command-line-arguments*))
  (format t "load-object-files trying to load ~a~%" object-files)
  (format t "clasp-build-mode = ~a~%" core:*clasp-build-mode*))

(export 'load-object-files)

(defun create-run-time-module-for-compile ()
  "Run time modules are used by COMPILE - a new one needs to be created for every COMPILE.
Return the module and the global variable that represents the load-time-value-holder as
\(value module global-run-time-values function-pass-manager\)."
  (llvm-create-module "compile"))

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
                 (core:fmt nil "str-{}" str))))
    (irc-const-gep2-64 %t*% str-gv 0 0 label)))


(defun module-make-global-string (str &optional (label ""))
  (declare (ignore label))
  "A function for creating unique strings within the module - return an LLVM pointer to the string"
  (or *the-module* (error "module-make-global-string-ptr *the-module* is NIL"))
  (let* ((unique-string-global-variable
          (llvm-sys:get-or-create-uniqued-string-global-variable
           *the-module* str (core:fmt nil ":::global-str-{}" str))))
    unique-string-global-variable))

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
  (let ((all (make-string-output-stream)))
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
  (declare (ignore lname))
  (if *current-source-pos-info*
      (let ((lineno (core:source-pos-info-lineno *current-source-pos-info*)))
        (cond
          (*compile-file-pathname*
           (core:fmt nil "___LAMBDA___{}.{}-{}^{}^{}"
                         (pathname-name *compile-file-pathname*)
                         (pathname-type *compile-file-pathname*)
                         *compile-file-unique-symbol-prefix*
                         lineno
                         (sys:next-number)))
          ;; Is this even possible?
          (*load-pathname*
           (core:fmt nil "{}.{}^{}^TOP-LOAD-{}"
                         (pathname-name *load-pathname*)
                         (pathname-type *load-pathname*)
                         lineno
                         (sys:next-number)))
          (t
           (core:fmt nil "UNKNOWN^{}^TOP-UNKNOWN" lineno))))
      "UNKNOWN??LINE^TOP-UNKNOWN"))

(defun jit-repl-function-name ()
  (sys:fmt nil "JITREPL-{}" (sys:next-number)))
  
(defun jit-startup-shutdown-function-names (module-id)
  (multiple-value-bind (startup-name linkage shutdown-name)
      (core:startup-linkage-shutdown-names module-id)
    (declare (ignore linkage))
    (values module-id startup-name shutdown-name))
  #+(or)(values module-id
                (sys:fmt nil "{}-{}" sys:*module-startup-function-name* module-id)
                (sys:fmt nil "{}-{}" sys:*module-shutdown-function-name* module-id)))

(export '(jit-startup-shutdown-function-names jit-repl-function-name))

(defun jit-function-name (lname)
  "Depending on the type of LNAME an actual LLVM name is generated"
  ;;  (break "Check backtrace")
  (cond
    ((pathnamep lname) (core:fmt nil "MAIN-{}" (string-upcase (pathname-name lname))))
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
    ;; (SETF symbol)
    ((and (consp lname) (eq (car lname) 'setf) (symbolp (second lname)))
;;;     (core:fmt t "jit-function-name handling SETF: {}%N" lname)
     (let* ((sn (cadr lname))
            (sym-pkg (symbol-package sn))
            (sym-name (symbol-name sn))
            (pkg-name (if sym-pkg
                          (string (package-name sym-pkg))
                          "KEYWORD")))
       (escape-and-join-jit-name (list sym-name pkg-name "SETF"))))
    ;; (SETF (symbol ...))
    ((and (consp lname) (eq (car lname) 'setf) (consp (second lname)))
;;;     (core:fmt t "jit-function-name handling SETFCONS: {}%N" lname)
     (let* ((sn (second lname))
            (sn-sym (first sn))
            (sym-pkg (symbol-package sn-sym))
            (sym-name (symbol-name sn-sym))
            (pkg-name (if sym-pkg
                          (string (package-name sym-pkg))
                          "KEYWORD")))
       (escape-and-join-jit-name (list sym-name pkg-name "SETFCONS"))))
    ;; (METHOD symbol . specializer-list): a method function
    ((and (consp lname) (eq (car lname) 'method) (symbolp (second lname)))
     (let* ((symbol (second lname))
            (sym-pkg (symbol-package symbol))
            (pkg-name (if sym-pkg
                      (string (package-name sym-pkg))
                      "UNINTERNED"))
           (name (symbol-name symbol))
           (specializers (core:fmt nil "{}" (cddr lname))))
       (escape-and-join-jit-name (list name pkg-name specializers "METHOD"))))
    ;; (METHOD (SETF symbol) . specializer-list): a method function
    ((and (consp lname) (eq (car lname) 'method) (consp (second lname)) (eq (car (second lname)) 'setf))
     (let* ((name-list (second lname))
            (setf-name-symbol (second name-list))
            (pkg-symbol (symbol-package setf-name-symbol))
            ;;; e.g. lname = (METHOD (SETF #:G4336) (T CONS))
            (pkg-name (if pkg-symbol
                          (string (package-name pkg-symbol))
                          "UNINTERNED"))
            (specializers (core:fmt nil "{}" (cddr lname))))
       (escape-and-join-jit-name (list (string setf-name-symbol) pkg-name specializers "SETFMETHOD"))))
    ;; (LAMBDA lambda-list): an anonymous function
    ((and (consp lname) (eq (car lname) 'cl:lambda))
     (jit-function-name 'cl:lambda))
    ;; (FLET name) or (LABELS name): a local function
    ((and (consp lname) (eq (car lname) 'cl:flet))
     (jit-function-name (second lname)))
    ((and (consp lname) (eq (car lname) 'cl:labels))
     (jit-function-name (second lname)))
    ;; Various little extensions for readability. See defmacro.lisp for occurrence.
    ((and (consp lname)
          (member (car lname) '(cl:macro-function cl:compiler-macro-function
                                ext::type-expander ext::setf-expander)))
     (jit-function-name (second lname)))
    #+(or) ;; uncomment this to be more forgiving
    ((consp lname)
     (core:fmt t "jit-function-name handling UNKNOWN: {}%N" lname)
     ;; What is this????
     (core:fmt nil "{}_CONS-LNAME?" lname))
    (t (error "Illegal lisp function name[~a]" lname))))
(export '(jit-function-name unescape-and-split-jit-name escape-and-join-jit-name))


(setq core:*llvm-function-name-hook* #'jit-function-name)


;;; ------------------------------------------------------------
;;;
;;;  JIT facility
;;;


(defun code-model (&key jit (target-faso-file *default-object-type*))
  (declare (ignore jit target-faso-file))
  "Return the code-model for the compilation mode"
  #+target-os-darwin 'llvm-sys:code-model-small
  #+(or target-os-freebsd target-os-linux) 'llvm-sys:code-model-small)

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
      (core:fmt t "Call-sites -> {}%N" call-sites))
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
               (declare (core:lambda-name with-track-llvm-time)
                        (ignore env))
               (let ((code (cdr args)))
                 `(do-track-llvm-time
                      (function
                       (lambda ()
                        ,@code)))))
	  t)

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

(eval-when (:load-toplevel :execute)
  (register-save-hook
   (function (lambda ()
     (format t "makunbound for *jit-pid* and *jit-log-stream*~%")
     (format t "makunbound for *jit-pid*~%")
     (makunbound '*jit-pid*)
     (format t "makunbound for *jit-log-stream*~%")
     (makunbound '*jit-log-stream*)
     (format t "Finished makunbound of some symbols~%")))))

(defun jit-register-symbol (symbol-name-string symbol-info &key verbose)
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
                (let ((filename (core:fmt nil "/tmp/perf-{}.map" (core:getpid))))
                  ;; when is not yet available
                  (if verbose
                    (core:fmt t "Writing jitted symbols to {}%N" filename))
                  (setq *jit-log-stream* (open filename :direction :output))))
               ;; If we are in a forked child then we need to create a new clasp-symbols-<pid> file and
               ;; refer to the parent clasp-symbols-<ppid> file.
               ((and *jit-log-stream* (not (= *jit-pid* (core:getpid))))
                (if verbose
                  (format t "Closing the *jit-log-stream* because the *jit-pid* ~d does not match our pid ~d ~%"
                          *jit-pid* (core:getpid)))
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

;;; It's too early for (in-package :cmp) to work but if we want slime to compile
;;; the code below we need it - so uncomment this as needed...

;;;(in-package :cmp)
(defparameter *dump-compile-module* nil)
(progn
  (export '(jit-add-module-return-function))
  (defparameter *jit-lock* (mp:make-recursive-mutex 'jit-lock))
  (defun jit-add-module-return-function (original-module main-fn startup-shutdown-id literals-list
                                         &key output-path name)
    (declare (ignore main-fn output-path))
    ;; Link the builtins into the module and optimize them
    #+(or)
    (progn
      (quick-module-dump original-module "before-link-builtins")
      (link-inline-remove-builtins original-module))
    (quick-module-dump original-module "module-before-optimize")
    (let ((module original-module))
      (irc-verify-module-safe module)
      (let ((jit-engine (llvm-sys:clasp-jit)))
        (multiple-value-bind (dummy-id startup-name shutdown-name)
            (jit-startup-shutdown-function-names startup-shutdown-id)
          (declare (ignore dummy-id))
          (let ((function (llvm-sys:get-function module startup-name)))
            (if (null function)
                (error "Could not obtain the startup function ~s by name" startup-name)))
          (with-track-llvm-time
              (unwind-protect
                   (progn
                     (if *dump-compile-module*
                         (progn
                           (core:fmt t "About to dump module%N")
                           (llvm-sys:dump-module module)
                           (core:fmt t "startup-name |{}|%N" startup-name)
                           (core:fmt t "Done dump module%N")
                           ))
                     (mp:get-lock *jit-lock*)
                     (if (member :dump-compile *features*) (llvm-sys:dump-module module))
                     (llvm-sys:add-irmodule jit-engine (llvm-sys:get-main-jitdylib jit-engine) module cmp:*thread-safe-context* startup-shutdown-id)
                     (llvm-sys:jit-finalize-repl-function jit-engine startup-name shutdown-name literals-list name))
                (progn
                  (gctools:thread-local-cleanup)
                  (mp:giveup-lock *jit-lock*)))))))))
