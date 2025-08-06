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
(in-package #:cmp)

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
     (disassemble-bytecode-function desig))
    (clos:funcallable-standard-object
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
     (format t "Disassembling function: ~a~%" desig)
     ;; This will (correctly) signal an error if the name is unbound.
     (disassemble (fdefinition desig) :type type))
    ((cons (eql lambda)) ; lambda expression (roughly)
     (ecase type
       ((:ir) (disassemble-to-ir desig))
       ((:asm) (disassemble (compile nil desig) :type type)))))
  nil)
