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

(defun safe-llvm-get-name (what)
  (llvm-sys:get-name what))

;;; Used by debugger - see clasp-debug:disassemble-frame
(defun disassemble-assembly (start end)
  (format t "; disassemble-assembly Size: ~s Origin: ~s~%" (- (core:pointer-integer end) (core:pointer-integer start)) start)
  (llvm-sys:disassemble-instructions (get-builtin-target-triple-and-data-layout)
                                     start end))

(defun disassemble-function-to-asm (function)
  (multiple-value-bind (symbol start end type)
      (core:lookup-address (core:function-pointer function))
    (disassemble-assembly start end)
    (bformat t "Done%N")))

;;; should work for both lambda expressions and interpreted functions.
(defun disassemble-to-ir (thing)
  (let* ((*save-module-for-disassemble* t)
         (cmp:*saved-module-from-clasp-jit* nil))
    (compile nil thing)
    (if cmp:*saved-module-from-clasp-jit*
        (format t "Disassembly: ~a~%" cmp:*saved-module-from-clasp-jit*)
        (error "Could not recover jitted module for ~a" thing)))
  (values))

(defun disassemble (desig &key (type :asm))
  "If type is :ASM (the default) then disassemble to machine assembly language.
If type is :IR then dump the LLVM-IR for all of the associated functions.
 Because Clasp does not normally store LLVM-IR for compiled functions,
 this case only works if a lambda expression or interpreted function is provided."
  (etypecase desig
    (compiled-function
     (ecase type
       ((:ir)
        (error "Dissassembly to LLVM-IR is not supported for already-compiled function: ~a"
               desig))
       ((:asm) (disassemble-function-to-asm desig))))
    ((or symbol (cons (eql setf) (cons symbol null))) ; function name
     (bformat t "Disassembling function: %s%N" desig)
     ;; This will (correctly) signal an error if the name is unbound.
     (disassemble (fdefinition desig) :type type))
    ((or (cons (eql lambda)) ; lambda expression (roughly)
         (satisfies core:interpreted-function-p))
     (ecase type
       ((:ir) (disassemble-to-ir desig))
       ((:asm) (disassemble-function-to-asm (compile nil desig))))))
  nil)
