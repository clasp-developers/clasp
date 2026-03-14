;;;
;;;    File: cmpwalk.lisp
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

;;
;; The code-walker-function must return a form or an altered form to continue compilation
;;
;; The macros that are invoked here must be the same macros that are used
;; in compile-file to set up the environment that the code needs to compile forms
;; as compile-file does.

(defun code-walk-using-bytecode (code-walker-function form env)
  (let* ((*code-walker* code-walker-function)
         (env (cond ; early, so no typecase yet
                ((null env) (make-null-lexical-environment))
                ((typep env 'lexenv) env)
                (t ; assume it's a cleavir environment. KLUDGE
                 (funcall (find-symbol "CLEAVIR-ENV->BYTECODE" "CLASP-CLEAVIR")
                          env))))
         (module (module/make)))
    (compile-lambda nil `(progn ,form) env module nil)))

(defun code-walk (code-walker-function form env)
  "Walk the form using whichever compiler we are currently using
   within env and call the code-walker-function on each internal form.
code-walker-function takes two arguments (form env).
Returns T if walked, NIL if not (e.g. because the compiler signaled an error)."
  (code-walk-using-bytecode code-walker-function form env))
