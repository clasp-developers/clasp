;;;
;;;    File: cmprepl.lsp
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
;; Don't use FORMAT here use BFORMAT 
;; otherwise you will have problems when format.lsp is bootstrapped

(in-package :clasp-cleavir)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up the cmp:*CLEAVIR-COMPILE-HOOK* so that COMPILE uses Cleavir
;;
(eval-when (:execute :load-toplevel)
  (setq cmp:*cleavir-compile-hook* 'cleavir-compile-t1expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up the cmp:*CLEAVIR-COMPILE-HOOK* so that COMPILE-FILE uses Cleavir
;;
(eval-when (:execute :load-toplevel)
  (setq cmp:*cleavir-compile-file-hook* 'cleavir-compile-file-form))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; cleavir-implicit-compile-hook - compile the form in the given environment
;;;

(eval-when (:execute :load-toplevel)
  (setq core:*eval-with-env-hook* 'cclasp-eval))


;;; These should be set up in Cleavir code
;;; Remove them once beach implements them
(defmethod cleavir-remove-useless-instructions:instruction-may-be-removed-p ((instruction cleavir-ir:rplaca-instruction))
  nil)

(defmethod cleavir-remove-useless-instructions:instruction-may-be-removed-p ((instruction cleavir-ir:rplacd-instruction))
  nil)


(defmethod cleavir-remove-useless-instructions:instruction-may-be-removed-p ((instruction cleavir-ir:set-symbol-value-instruction)) nil)






(defparameter *simple-environment* nil)
(defvar *code-walker* nil)
(export '(*simple-environment* *code-walker*))

(defun mark-env-as-function ()
  (push 'si::function-boundary *simple-environment*))

(defun local-function-form-p (form)
  #+clc(warn "Convert this to use predicate ext:local_function_p")
  (and (listp form) (member (first form) '(flet labels))))

(defmethod cleavir-generate-ast:convert :around (form environment (system clasp-64bit))
  (declare (ignore system))
  (let ((*simple-environment* *simple-environment*))
    (when *code-walker*
      (when (local-function-form-p form)
        (mark-env-as-function))
      (funcall *code-walker* form *simple-environment*))
    (call-next-method)))

(defun code-walk-for-method-lambda-closure (form env &key code-walker-function)
  (let* ((cleavir-generate-ast:*compiler* 'cl:compile)
         (clasp-cleavir:*code-walker* code-walker-function))
    (cleavir-generate-ast:generate-ast form env *clasp-system*)))

(export 'code-walk-for-method-lambda-closure)


