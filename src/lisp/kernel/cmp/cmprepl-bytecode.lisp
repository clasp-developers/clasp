;;;
;;;    File: cmprepl.lisp
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
;; Insert the compiler into the repl
;;
;; Don't use FORMAT here use core:fmt
;; otherwise you will have problems when format.lisp is bootstrapped


#+(or)
(eval-when (:compile-toplevel :execute)
  (setq core:*debug-eval* t))

(defun trace-compiler ()
  (format t "Turning on safe-trace of compiler functions~%")
  (core:safe-trace
   cmp::irc-typed-gep
   literal::do-literal-table
   cmp::maybe-spill-to-register-save-area
   cmp::codegen-startup-shutdown
   literal::constants-table-reference
   cmp::compile-to-module-with-run-time-table
   cmp::bclasp-compile*
   cmp::compile-with-hook
   cmp::compile-in-env
   compile
   load
   cmp::bclasp-implicit-compile-repl-form
   cmp::irc-const-gep2-64
   literal::constants-table-value
   cmp::gen-defcallback
   cmp::irc-rack-slot-address
   cmp::irc-value-frame-reference
   cmp::irc-array-dimension
   cmp::irc-header-stamp
   cmp::irc-calculate-entry
   cmp::irc-calculate-real-args
   cmp::compile-lambda-list-code
   cmp::c++-field-ptr
   cmp::layout-xep-function*
   cmp::layout-xep-function
   cmp::bclasp-compile-lambda-list-code
   cmp::layout-xep-group
   cmp::do-new-function
   literal::do-rtv
   llvm-sys:make-global-variable
   ))

(in-package :cmp)

(defparameter *print-implicit-compile-form* nil)



(defun do-bytecompile (form env)
  #-use-cmpref (cmp:bytecompile form env)
  #+use-cmpref (cmpref:bytecompile form env)
  )


#+(or)
(defun bclasp-implicit-compile-repl-form (form &optional environment)
  (declare (core:lambda-name cmp-repl-implicit-compile))
  (when *print-implicit-compile-form* 
    (core:fmt t "Compiling form: {}%N" form)
    (core:fmt t "*active-protection* --> {}%N" cmp::*active-protection*))
  (let ((repl-name (intern (core:fmt nil "repl-form-{}" (core:next-number)) :core)))
    (funcall
     (core:with-memory-ramp (:pattern 'gctools:ramp)
       (compile-in-env `(lambda ()
                          (declare (core:lambda-name ,repl-name))
                          ,form)
                       environment
                       nil
                       *default-compile-linkage*)))))

(defun bytecode-implicit-compile-repl-form (form &optional env)
  (declare (core:lambda-name cmp-repl-implicit-compile))
  (when (null env)
    (setf env (make-null-lexical-environment)))
  (when *print-implicit-compile-form*
    (core:fmt t "Compiling form: {}%N" form)
    (core:fmt t "*active-protection* --> {}%N" cmp::*active-protection*))
  (let ((repl-name (intern (core:fmt nil "repl-form-{}" (core:next-number)) :core)))
    (funcall (do-bytecompile
              `(lambda ()
                 (declare (core:lambda-name ,repl-name))
                 (progn ,form))
              env))))

;;;
;;; Don't install the bootstrapping compiler as the implicit compiler when compiling cleavir
;;;
;;; When debugging the aclasp/bclasp compiler
;;; you might not want implicit compilation of repl forms
;;;   the no-implicit-compilation *feature* controls this.
;;;   Don't add this feature if you want implicit compilation
;;;
;;; 
#-(or no-implicit-compilation)
(setq *implicit-compile-hook* 'bytecode-implicit-compile-repl-form)

(defun bytecode-toplevel-progn (forms env)
  (if (null forms)
      nil
      (do ((forms forms (cdr forms)))
          ((null (cdr forms))
           (return-from bytecode-toplevel-progn
             (bytecode-toplevel-eval (car forms) env)))
        (bytecode-toplevel-eval (car forms) env))))

(defun bytecode-toplevel-eval-when (situations forms env)
  (if (or (member :execute situations) (member 'cl:eval situations))
      (bytecode-toplevel-progn forms env)
      nil))

(defun bytecode-toplevel-locally (body env)
  (multiple-value-bind (decls body docs specials)
      (core:process-declarations body nil)
    (declare (ignore decls docs))
    (let* ((env (or env (make-null-lexical-environment)))
           (new-env
             (if specials
                 (lexenv/add-specials env specials)
                 env)))
      (bytecode-toplevel-progn body new-env))))

(defun bytecode-toplevel-macrolet (bindings body env)
  (let ((macros nil)
        (env (or env (make-null-lexical-environment))))
    (dolist (binding bindings)
      (let* ((name (car binding)) (lambda-list (cadr binding))
             (body (cddr binding))
             (eform (ext:parse-macro name lambda-list body env))
             (aenv (lexenv/macroexpansion-environment env))
             (expander (do-bytecompile eform aenv))
             (info (cmp:local-macro-info/make expander)))
        (push (cons name info) macros)))
    (bytecode-toplevel-locally
     body (make-lexical-environment
           env :funs (append macros (cmp:lexenv/funs env))))))

(defun bytecode-toplevel-symbol-macrolet (bindings body env)
  (let ((smacros nil) (env (or env (make-null-lexical-environment))))
    (dolist (binding bindings)
      (push (cons (car binding) (make-symbol-macro-var-info (cadr binding)))
            smacros))
    (bytecode-toplevel-locally
     body (make-lexical-environment
           env
           :vars (append (nreverse smacros) (cmp:lexenv/vars env))))))

(defun bytecode-toplevel-eval (form env)
  (let ((form (macroexpand form env)))
    (if (consp form)
        (case (car form)
          ((progn) (bytecode-toplevel-progn (cdr form) env))
          ((eval-when) (bytecode-toplevel-eval-when (cadr form) (cddr form) env))
          ((locally) (bytecode-toplevel-locally (cdr form) env))
          ((macrolet) (bytecode-toplevel-macrolet (cadr form) (cddr form) env))
          ((symbol-macrolet)
           (bytecode-toplevel-symbol-macrolet (cadr form) (cddr form) env))
          (otherwise
           (bytecode-implicit-compile-repl-form form env)))
        (bytecode-implicit-compile-repl-form form env))))

(setq *eval-with-env-hook* 'bytecode-toplevel-eval)

;;#+(and clasp-min (not no-implicit-compilation))
#+(or)
(eval-when (:execute)
  ;; Load the compiler and the file compiler in aclasp
  ;; lets see if that speeds up the compilation
  (load "sys:src;lisp;kernel;cmp;compiler.lisp" :print t)
  (load "sys:src;lisp;kernel;cmp;compilefile.lisp" :print t))

#+(or)
(eval-when (:execute)
  (core:fmt t "!%N!%N!\n! cmprepl.lisp has (setq cmp:*debug-dump-module* t)\n!\n!\n!  TURN IT OFF AGAIN\n!\n")
  (setq cmp:*debug-dump-module* t)
  )

(defmacro with-interpreter (&body body)
  "Run the body using the interpreter"
  `(let ((core:*eval-with-env-hook* #'core:interpret-eval-with-env))
    ,@body))


