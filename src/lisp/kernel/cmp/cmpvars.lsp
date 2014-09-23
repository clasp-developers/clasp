;;;
;;;    File: cmpvars.lsp
;;;

;; Copyright (c) 2014, Christian E. Schafmeister
;; 
;; CLASP is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Library General Public
;; License as published by the Free Software Foundation; either
;; version 2 of the License, or (at your option) any later version.
;; 
;; See file 'clasp/Copyright' for full details.
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


(in-package :compiler)

;;
;; variable lookups are in this file so we can compile-file it first and make 
;; compilation faster
;;

(defun codegen-special-var-lookup (result sym env)
  "Return IR code that returns the value cell of a special symbol"
  (cmp-log "About to codegen-special-var-lookup symbol[%s]\n" sym)
  (if (eq sym 'nil)
      (codegen-literal result nil env)
      (let ((global-symbol-ptr (irc-global-symbol sym env)))
	(cmp-log "About to invoke create-call2 - global-symbol-ptr --> %s\n" global-symbol-ptr)
	(irc-intrinsic "symbolValueRead" result global-symbol-ptr))))




(defun codegen-lexical-var-lookup (result depth-index env)
  "Generate IR for lookup of lexical value in runtime-env using depth and index"
  (let* ((depth (car depth-index))
	 (index (cadr depth-index))
	 (runtime-env (irc-renv env)))
    (dbg-set-current-debug-location-here)
    (cmp-log "About to call lexicalValueRead depth-index[%s] depth[%d] index[%d]\n" depth-index depth index)
    (irc-intrinsic "lexicalValueRead" result (jit-constant-i32 depth) (jit-constant-i32 index) runtime-env))
  result)


(defun codegen-var-lookup (result sym old-env)
  "Return IR code thsym returns the value of a symbol that is either lexical or special"
  (let ((classified (irc-classify-variable old-env sym)))
    (cmp-log "About to codegen-var-lookup for %s - classified as: %s\n" sym classified)
    (if (eq (car classified) 'ext:special-var)
	(codegen-special-var-lookup result sym old-env)
	(let ((depth-index (cddr classified)))
	  (codegen-lexical-var-lookup result depth-index old-env)))))


(defun codegen-symbol-value (result symbol env)
  (if (keywordp symbol)
      (irc-intrinsic "symbolValueRead" result (irc-global-symbol symbol env))
      (let ((expanded (macroexpand symbol env)))
	(if (eq expanded symbol)
	    ;; The symbol is unchanged, look up its value
	    (codegen-var-lookup result symbol env)
	    ;; The symbol was a symbol-macro - evaluate it
	    (codegen result expanded env)
	    ))))
	
