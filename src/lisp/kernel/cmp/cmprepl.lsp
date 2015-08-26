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

(in-package :cmp)


;;;
;;; Don't install the bootstrapping compiler as the implicit compiler when compiling cleavir
;;;

(defparameter *print-implicit-compile-form* nil)

(defun bclasp-implicit-compile-form (form &optional environment)
  (declare (core:lambda-name cmp-repl-implicit-compile))
  (when *print-implicit-compile-form* 
    (bformat t "Compiling form: %s\n" form))
  (with-compilation-unit (:override t)
    (multiple-value-bind (compiled-function warn fail)
        (compile-in-env nil `(lambda () 
                               (declare (core:lambda-name implicit-repl))
                               ,form) environment nil)
      (funcall compiled-function))))
;;    (values compiled-function warn fail)))

#-cleavir  
(setq *implicit-compile-hook* #'bclasp-implicit-compile-form)

