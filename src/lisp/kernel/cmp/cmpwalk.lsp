;;;
;;;    File: cmpwalk.lsp
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

(defun code-walk-using-compiler (form env &key code-walker-function)
  "This is used in clos/method.lsp to code walk defmethod bodies"
  (let* ((module (llvm-create-module "code-walk-for-defmethod"))
	 (fpm #+(or)(create-function-pass-manager-for-compile-file module))
	 (*code-walker* code-walker-function))
    (define-primitives-in-module module)
    (with-compilation-unit ()
      (with-module (:module module :function-pass-manager fpm)
        (let ((*gv-source-path-name* (jit-make-global-string-ptr "code-walk-using-compiler" "source-path-name"))
              (*gv-source-file-info-handle* (llvm-sys:make-global-variable *the-module*
                                                                           +i32+ ; type
                                                                           nil ; constant
                                                                           'llvm-sys:internal-linkage
                                                                           (jit-constant-i32 0)
                                                                           "source-file-info-handle"))
              )
          (with-load-time-value-unit (ltv-init-fn)
            (compile-in-env nil form env)))
        (llvm-sys::module-delete module)
        ))))


(export 'code-walk-using-compiler)
