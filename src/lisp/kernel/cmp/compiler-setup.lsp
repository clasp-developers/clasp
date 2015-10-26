;;;
;;;    File: compiler-setup.lsp
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

(defvar *engine-builder* (llvm-sys:make-engine-builder *the-module*))
;; NOTE:  *the-module* becomes invalid after call to make-engine-builder - is this OK?



;;
;; --------       Here set the execution engine kind
;;                INTERPRETER or JIT (for native code)
;;
;;(llvm-sys:set-engine-kind *engine-builder* 'llvm-sys:interpreter)
(let ((target-options (llvm-sys:make-target-options)))
;;  (llvm-sys:setf-no-frame-pointer-elim target-options t)
;;  (llvm-sys:setf-jitemit-debug-info target-options t)
       ;; module is invalid after make-engine-builder call
  (llvm-sys:set-target-options *engine-builder* target-options)
(defvar *the-execution-engine* (llvm-sys:create *engine-builder*))
(if (is-undefined *the-execution-engine*)
  (error "The execution engine could not be created: ~a" (llvm-sys:error-string *engine-builder*)))
