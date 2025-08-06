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

(in-package #:cmp)

(defconstant +debug-dwarf-version+ 5)

(export '*primitives*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Thread-local special variables to support the LLVM compiler
;;;
;;; To allow LLVM to run in different threads, certain things need to be thread local
;;;
(eval-when (:load-toplevel :execute)
  (mp:push-default-special-binding 'cmp:*thread-safe-context* '(llvm-sys:create-thread-safe-context))
  (mp:push-default-special-binding '*debugger-hook* nil)
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

(defun parse-bitcode (filename context &key print output-type)
  ;; Load a module from a bitcode or .ll file
  (cond
    ((eq output-type :fasobc)
     (if print (core:fmt t "Loading {}%N" filename))
     (llvm-sys:parse-bitcode-file filename context))
    ((eq output-type :fasoll)
     (if print (core:fmt t "Loading {}%N" filename))
     (llvm-sys:parse-irfile filename context))
    (t (error "Add support for output-type ~a" output-type))))

(export '(write-bitcode parse-bitcode load-ir-run-c-function))

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
                                           output-type)
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
                                              :output-type output-type)))
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
  (format t "output-type = ~a~%" cmp:*default-output-type*))

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

(defun potentially-save-module ()
  (when *save-module-for-disassemble*
    (setq *saved-module-from-clasp-jit*
          (with-output-to-string (*standard-output*)
            (llvm-sys:dump-module *the-module* *standard-output*)))))

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

(defun jit-repl-function-name ()
  (sys:fmt nil "JITREPL-{}" (sys:next-number)))
  
(defun jit-startup-shutdown-function-names (module-id)
  (multiple-value-bind (startup-name linkage shutdown-name)
      (core:startup-linkage-shutdown-names module-id)
    (declare (ignore linkage))
    (values startup-name shutdown-name))
  #+(or)(values module-id
                (sys:fmt nil "{}-{}" sys:*module-startup-function-name* module-id)
                (sys:fmt nil "{}-{}" sys:*module-shutdown-function-name* module-id)))

(export '(jit-startup-shutdown-function-names jit-repl-function-name))


(export '(unescape-and-split-jit-name))

;;; ------------------------------------------------------------
;;;
;;;  JIT facility
;;;


(defun code-model (&key jit (output-type *default-output-type*))
  (declare (ignore jit output-type))
  "Return the code-model for the compilation mode"
  'llvm-sys:code-model-small)

(export 'code-model)

(defun do-track-llvm-time (closure)
  "Run the closure in and keep track of the time, adding it to this threads accumulated llvm time"
  (let ((start-llvm-time (get-internal-run-time)))
    (unwind-protect
         (funcall closure)
      (let ((llvm-time (/ (- (get-internal-run-time) start-llvm-time) (float internal-time-units-per-second))))
        (llvm-sys:accumulate-llvm-usage-seconds llvm-time)))))

(defmacro with-track-llvm-time (&body code)
  `(do-track-llvm-time (lambda () (progn ,@code))))
