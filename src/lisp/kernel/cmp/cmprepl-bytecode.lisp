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
   cmp::compile-with-hook
   cmp::compile-in-env
   compile
   load
   cmp::bclasp-implicit-compile-repl-form
   cmp::irc-const-gep2-64
   literal::constants-table-value
   cmp::gen-defcallback
   cmp::irc-rack-slot-address
   cmp::irc-array-dimension
   cmp::irc-header-stamp
   cmp::irc-calculate-entry
   cmp::irc-calculate-real-args
   cmp::compile-lambda-list-code
   cmp::c++-field-ptr
   literal::do-rtv
   llvm-sys:make-global-variable
   ))

(in-package :cmp)


(defun do-bytecompile (form env)
  (cmp:bytecompile form env)
  ;;#-use-cmpref (cmp:bytecompile form env)
;;  #+use-cmpref (cmpref:bytecompile form env)
  )

(defmacro with-interpreter (&body body)
  "Run the body using the interpreter"
  `(let ((core:*eval-with-env-hook* #'core:interpret-eval-with-env))
    ,@body))


