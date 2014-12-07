;;;
;;;    File: cmprepl.lsp
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
;; Don't use FORMAT here use BFORMAT 
;; otherwise you will have problems when format.lsp is bootstrapped

(in-package :cmp)

  
(setq *implicit-compile-hook*
      (compile nil '(lambda (form &optional environment)
;;		     (bformat t "*implicit-compile-hook* *load-truename* = %s   compiling form: %s\n" *load-truename* form)
                     (multiple-value-bind (compiled-function warn fail)
                         (compile-in-env nil `(lambda () ,form) environment)
;;                         (compile-in-env nil form environment)
                       (values compiled-function warn fail)))))

;; From now on every S-exp is compiled before evaluation
(setq *implicit-compilation* t)
