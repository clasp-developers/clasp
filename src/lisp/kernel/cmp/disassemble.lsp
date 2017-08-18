;;;
;;;    File: disassemble.lsp
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
(in-package :cmp)

(defun disassemble (desig &key (start 0) (num 16) (type :IR))
  "If type is :ASM then disassemble to assembly language from the START instruction, disassembling NUM instructions
   if type is :IR then dump the llvm-ir for all of the associated functions and ignore START and NUM"
  (multiple-value-bind (func-or-lambda name)
      (cond
        ((null desig) (error "No function provided"))
	((symbolp desig) (if (fboundp desig)
			     (values (fdefinition desig) desig)
			     (error "No function bound to ~A" desig)))
	((functionp desig) (multiple-value-bind (fn-lambda closurep name)
			       (function-lambda-expression desig)
			     (values desig name)))
	(t (error "Unknown argument ~a passed to disassemble" desig)))
    (setq name (if name name 'lambda))
    (bformat t "Disassembling function: %s\n" (repr func-or-lambda))
    (cond
      ((functionp func-or-lambda)
       (let ((fn func-or-lambda))
	 (cond
	   ((compiled-function-p fn)
            (if (eq type :asm)
                (llvm-sys:disassemble-instructions (llvm-sys:get-default-target-triple) (core:function-pointer fn) start num)
                (llvm-sys:disassemble* fn)))
	   ((interpreted-function-p fn)
	    (format t "This is a interpreted function - compile it first~%"))
           ((eq type :asm)
                (llvm-sys:disassemble-instructions (llvm-sys:get-default-target-triple) (core:function-pointer fn) start num))
	   (t (error "Unknown target for disassemble: ~a" fn)))))
      ((and (consp desig) (or (eq (car desig) 'lambda) (eq (car desig) 'ext::lambda-block)))
       (let* ((*all-functions-for-one-compile* nil)
              (funcs (codegen-closure nil desig nil)))
	 (dolist (i *all-functions-for-one-compile*)
	   (llvm-sys:dump i))))
      (t (error "Cannot disassemble"))))
  nil)
