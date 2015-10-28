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

(in-package :clasp-cleavir)

(eval-when (:execute :load-toplevel)
  (setq core:*eval-with-env-hook* 'cclasp-eval))

(defparameter *my-env* nil)
(defvar *code-walker* nil)
(export '(*my-env* *code-walker*))

(in-package :cleavir-generate-ast)
(defmethod cleavir-generate-ast:convert :around (form environment (system clasp-cleavir::clasp-64bit))
  (declare (ignore system))
  (if clasp-cleavir:*code-walker*
      (let ((clasp-cleavir:*my-env*
             (if (and (consp form) (member (car form) '(cl:flet cl:labels)))
                 (cons 'si::function-boundary clasp-cleavir:*my-env*)
                 clasp-cleavir:*my-env*)))
        (funcall clasp-cleavir:*code-walker* form clasp-cleavir:*my-env*)
        (format t "form: ~a~%" form)
        (format t "    *my-env*: ~a~%" clasp-cleavir::*my-env*)
        (call-next-method))
      (call-next-method)))

(in-package :clasp-cleavir)

(defun code-walk-for-method-lambda-closure (form env &key code-walker-function)
  (let* ((cleavir-generate-ast:*compiler* 'cl:compile)
         (clasp-cleavir:*code-walker* code-walker-function))
    (cleavir-generate-ast:generate-ast form env *clasp-system*)))

(export 'code-walk-for-method-lambda-closure)


#+(or)(defun cleavir-implicit-compile-hook (form &optional environment)
        (declare (core:lambda-name cmp-repl-implicit-compile))
        #+(or)(bformat t "*implicit-compile-hook* *load-truename* = %s   compiling form: %s\n" *load-truename* form)
        (let ((cmp:*cleavir-compile-hook* #'cleavir-compile-t1expr))
          (with-compilation-unit (:override t)
            (multiple-value-bind (compiled-function warn fail)
                (cmp:compile-in-env
                 nil
                 `(lambda () 
                    (declare (core:lambda-name implicit-repl))
                    ,form) environment cmp:*cleavir-compile-hook*)
              (funcall compiled-function)))))

#+(or)(eval-when (:execute :load-toplevel)
        (setq cmp:*implicit-compile-hook* #'cleavir-implicit-compile-hook))


