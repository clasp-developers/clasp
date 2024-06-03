;;;
;;;    File: disassemble.lisp
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

(defun safe-llvm-get-name (what)
  (llvm-sys:get-name what))

;;; Used by debugger - see clasp-debug:disassemble-frame
(defun disassemble-assembly (start end)
  (format t "~&; disassemble-assembly Size: ~s Origin: ~s~%" (- (core:pointer-integer end) (core:pointer-integer start)) start)
  (llvm-sys:disassemble-instructions (get-builtin-target-triple-and-data-layout)
                                     start end))

(defun disassemble-function-to-asm (function)
  (let ((function-pointers (core:function-pointer-alist function)))
    (dolist (fp function-pointers)
      (let ((entry-point-name (car fp))
            (address (cdr fp)))
        (when address
          (multiple-value-bind (symbol start end)
              (core:lookup-address address)
            (if symbol
                (progn
                  (format t "Entry point ~a~%" (if (fixnump entry-point-name)
                                                   (format nil "xep~a" entry-point-name)
                                                   (string entry-point-name)))
                  (disassemble-assembly start end))
                (format t "; could not locate code object (bug?)~%"))))))))

(defun potentially-save-module (&optional (module *the-module*))
  (when *save-module-for-disassemble*
    (setq *saved-module-from-clasp-jit*
          (with-output-to-string (*standard-output*)
            (llvm-sys:dump-module module *standard-output*)))))

;;; should work for both lambda expressions and interpreted functions.
(defun disassemble-to-ir (thing)
  (let* ((*save-module-for-disassemble* t)
         (cmp:*saved-module-from-clasp-jit* nil))
    (compile nil thing)
    (if cmp:*saved-module-from-clasp-jit*
        (format t "~&Disassembly: ~a~%" cmp:*saved-module-from-clasp-jit*)
        (error "Could not recover jitted module for ~a" thing)))
  (values))

(defun disassemble (desig &key (type :asm))
  "If type is :ASM (the default) then disassemble to assembly language.
If type is :IR then dump the LLVM-IR for all of the associated functions.
 Because Clasp does not normally store LLVM-IR for compiled functions,
 this case only works if a lambda expression or interpreted function is provided."
  (etypecase desig
    (core:closure
     ;; If given a closure, disassemble the underlying simple function.
     ;; This allows both bytecode and native-compiled closures to be
     ;; disassembled correctly.
     (disassemble (core:function/entry-point desig) :type type))
    (core:bytecode-simple-fun
     (unless (eq type :asm)
       (error "Only disassembly to bytecode is supported for bytecode function: ~a" desig))
     (cmpref:disassemble-bytecode-function desig))
    (core:funcallable-instance
     (disassemble (clos:get-funcallable-instance-function desig) :type type))
    (core:gfbytecode-simple-fun
     (unless (eq type :asm)
       (error "Only disassembly to bytecode is supported for bytecode discriminating function: ~a" desig))
     ;; Defined later in clos/dtree.lisp.
     (clos::disassemble-discriminator desig))
    ((or compiled-function core:simple-fun)
     (ecase type
       ((:ir)
        (error "Dissassembly to LLVM-IR is not supported for already-compiled function: ~a"
               desig))
       ((:asm) (disassemble-function-to-asm desig))))
    ((or symbol (cons (eql setf) (cons symbol null))) ; function name
     (core:fmt t "Disassembling function: {}%N" desig)
     ;; This will (correctly) signal an error if the name is unbound.
     (disassemble (fdefinition desig) :type type))
    ((cons (eql lambda)) ; lambda expression (roughly)
     (ecase type
       ((:ir) (disassemble-to-ir desig))
       ((:asm) (disassemble-function-to-asm (compile nil desig))))))
  nil)
