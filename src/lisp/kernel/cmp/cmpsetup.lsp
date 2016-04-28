;;;
;;;    File: cmpsetup.lsp
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

(in-package :cmp)

(defvar *debug-attach-debugging-info-to-value-frames* t)


;; Use the DebugIRPass to add self referencing debugging informato to IR
(defvar *debug-ir* nil)


;; Turn on all sorts of debug printing within the compiler
;;
(defvar *debug-compiler* nil)
(export '*debug-compiler*)

;;; Turn these on to verify llvm modules and functions
(defvar *verify-llvm-modules* nil)
(defvar *verify-llvm-functions* nil)


(defvar *dump-module-on-completion* nil)




;; Generate a bitcode file for the llvm-ir prior to running optimization passes on it
;;
(defvar *debug-generate-prepass-llvm-ir* nil)

;; Generate C++ destructors for reference-counting otherwise don't
;;

(defvar *compiler-suppress-dtors* #+use-refcount nil #-use-refcount t)
(export '*compiler-suppress-dtors*)


;;
;;
;; Insert low-level tracing calls within the generated code at specific points
;;
;; 
;;
(defvar *low-level-trace* nil
  "Insert tracing calls within the generated code at specific points.
Set its value to a list of symbols that indicate where the tracing calls will be inserted.
Options are :tagbody :go :all :eh-landing-pads
" )

(defvar *low-level-trace-print* nil
  "Controls whether low-level trace id's are printed(T) or not(nil) as they are encountered.")


;;
;; Keep track of with-try basic blocks
;;
(defparameter *next-try-id* 0)


;; 
;; Name basic-blocks according to what with-try block they are in
;;
(defparameter *block-name-prefix* "")



;;
;; Calculate the data layout for the system
;;

(defvar *data-layout* nil)
(let* ((module (llvm-create-module (next-run-time-module-name)))
       (engine-builder (llvm-sys:make-engine-builder module))
       (target-options (llvm-sys:make-target-options)))
;;  (llvm-sys:setf-no-frame-pointer-elim target-options t)
;;  (llvm-sys:setf-jitemit-debug-info target-options t)
;;  (llvm-sys:setf-jitemit-debug-info-to-disk target-options t)
       ;; module is invalid after make-engine-builder call
  (llvm-sys:set-target-options engine-builder target-options)
  (let* ((execution-engine (llvm-sys:create engine-builder)))
    (setq *data-layout* (llvm-sys:get-data-layout execution-engine))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Turn off compiler debugging code once we are confident it works
;;;
;;;
#-(or)
(progn
  (defmacro debug-print-i32 (num) nil)
  (defmacro cmp-log-dump (fn) nil)
  (defmacro cmp-log (fmt &rest args ) nil)
  (defun is-debug-compiler-on () nil)
  )


(progn
  (defun is-debug-compiler-on ()
    *debug-compiler*)
  (defmacro debug-print-i32 (num)
    `(if (is-debug-compiler-on)
	 (irc-intrinsic "debugPrintI32" (jit-constant-i32 ,num))
	 nil))
  (defmacro cmp-log (fmt &rest args)
    `(if (is-debug-compiler-on)
	 (progn
	   (bformat t "%s:%s " (source-file-name) (source-line-column))
	   (bformat t ,fmt ,@args))
	 nil))
  (defmacro cmp-log-dump (fn-or-module)
    `(if (is-debug-compiler-on)
	 (llvm-sys:dump ,fn-or-module)
	 nil))
  )


;; When Cleavir is installed set the value of *cleavir-compile-hook* to use it to compile forms
;; It expects a function of one argument (lambda (form) ...) that will generate code in the
;; current *module* for the form.  The lambda returns T if cleavir succeeded in compiling the form
;; and nil otherwise
(defvar *cleavir-compile-hook* nil)
(defvar *cleavir-compile-file-hook* nil)


