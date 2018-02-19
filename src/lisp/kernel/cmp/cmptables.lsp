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
    (progn codegen-progn convert-progn)
    (if codegen-if convert-if)
    (block  codegen-block convert-block)
    (return-from  codegen-return-from convert-return-from)
    (setq codegen-setq convert-setq)
    (let codegen-let convert-let)
    (let* codegen-let* convert-let*)
    (function  codegen-function convert-function)
    (tagbody codegen-tagbody convert-tagbody)
    (go codegen-go convert-go)
    (multiple-value-call  codegen-multiple-value-call convert-multiple-value-call)
    (multiple-value-prog1  codegen-multiple-value-prog1 convert-multiple-value-prog1)
    (flet  codegen-flet convert-flet)
    (labels  codegen-labels convert-labels)
    (eval-when  codegen-eval-when convert-eval-when)
    (the  codegen-the convert-the convert-the convert-the)
    (core:truly-the  codegen-truly-the convert-truly-the)
    (locally  codegen-locally convert-locally)
    (quote  codegen-quote convert-quote)
    (macrolet  codegen-macrolet convert-macrolet)
    (dbg-i32  codegen-dbg-i32 convert-dbg-i32)
    (load-time-value  codegen-load-time-value convert-load-time-value)
    (core:multiple-value-foreign-call codegen-multiple-value-foreign-call convert-multiple-value-foreign-call)
    (core:foreign-call codegen-foreign-call convert-foreign-call)
    (core:foreign-call-pointer codegen-foreign-call-pointer convert-foreign-call-pointer)
    (symbol-macrolet  codegen-symbol-macrolet convert-symbol-macrolet)
    (llvm-inline codegen-llvm-inline convert-llvm-inline)
    (:gc-profiling codegen-gc-profiling convert-gc-profiling)
    (core::debug-message codegen-debug-message convert-debug-message)
    ;; Handled with macros and funcalls
    #+(or)(unwind-protect .  codegen-unwind-protect)
    (catch  codegen-catch convert-catch)
    (throw  codegen-throw convert-throw)
    (progv  codegen-progv convert-progv)
    ))

(defun make-dispatch-table (alist)
  (let ((hash (make-hash-table :size (max 128 (* 2 (length alist))) :test #'eq)))
    (dolist (entry alist)
      (let ((name (first entry))
	    (codegen-function (second entry))
            (convert-function (third entry)))
	(core::hash-table-setf-gethash hash name (list convert-function codegen-function))))
    hash))

(defvar *special-operator-dispatch* (make-dispatch-table +special-operator-dispatch+))

#+debug-mps
(progn
  (bformat t "Dumping *special-operator-dispatch* = %s%N" *special-operator-dispatch*)
  (maphash #'(lambda (k v) (bformat t "Special operator = %s%N" k )) *special-operator-dispatch*))
