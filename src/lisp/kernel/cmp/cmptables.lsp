;;;
;;;    File: cmptables.lsp
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
(export 'llvm-inline)

(defconstant +special-operator-dispatch+
  '(
    (progn codegen-progn mincomp-progn)
    (setq codegen-setq)
    (let codegen-let)
    (let* codegen-let*)
    (if codegen-if mincomp-if)
    (function  codegen-function)
    (block  codegen-block)
    (return-from  codegen-return-from)
    (tagbody codegen-tagbody)
    (go codegen-go)
    (multiple-value-call  codegen-multiple-value-call)
    (multiple-value-prog1  codegen-multiple-value-prog1)
    (flet  codegen-flet)
    (labels  codegen-labels)
    (eval-when  codegen-eval-when)
    (the  codegen-the mincomp-the)
    (core:truly-the  codegen-truly-the)
    (locally  codegen-locally)
    (quote  codegen-quote)
    (macrolet  codegen-macrolet)
    (dbg-i32  codegen-dbg-i32)
    (load-time-value  codegen-load-time-value)
    (core:intrinsic-call codegen-intrinsic-call)
    (symbol-macrolet  codegen-symbol-macrolet)
    (cmp:llvm-inline codegen-llvm-inline)
    (cmp::gc-profiling codegen-gc-profiling)
    (core::debug-message codegen-debug-message)
    ;; Handled with macros and funcalls
    #+(or)(unwind-protect .  codegen-unwind-protect)
    (catch  codegen-catch)
    (throw  codegen-throw)
    (progv  codegen-progv)
    ))


(defun make-dispatch-table (alist)
  (let ((hash (make-hash-table :size (max 128 (* 2 (length alist))) :test #'eq)))
    (dolist (entry alist)
      (let ((name (car entry))
	    (codegen-function (cadr entry))
            (mincomp-function (caddr entry)))
	(core::hash-table-setf-gethash hash name (list mincomp-function codegen-function))))
    hash))

(defvar *special-operator-dispatch* (make-dispatch-table +special-operator-dispatch+))

#+debug-mps
(progn
  (bformat t "Dumping *special-operator-dispatch* = %s\n" *special-operator-dispatch*)
  (maphash #'(lambda (k v) (bformat t "Special operator = %s\n" k )) *special-operator-dispatch*))
